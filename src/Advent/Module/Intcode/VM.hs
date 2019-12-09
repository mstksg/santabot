{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ViewPatterns              #-}


module Advent.Module.Intcode.VM (
    Memory(..)
  , VM
  , stepForever
  , maybeToEither
  , runProg
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Conduino
import           Data.Map                  (Map)
import           Data.Traversable
import           Data.Void
import           Linear
import           Numeric.Natural
import qualified Data.Conduino.Combinators as C
import qualified Data.Map                  as M

type VM = Pipe Int Int Void (StateT Memory (Either String)) ()

data Memory = Mem
    { mFuel :: Int
    , mPos  :: Natural
    , mBase :: Int
    , mRegs :: Map Natural Int
    }
  deriving Show

data Mode = Pos | Imm | Rel
  deriving (Eq, Ord, Enum, Show)

data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | ChB | Hlt
  deriving (Eq, Ord, Enum, Show)

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add .. ChB]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

toNatural' :: MonadError String m => Int -> m Natural
toNatural' x
    | x < 0     = throwError $ "invalid position: " ++ show x
    | otherwise = pure $ fromIntegral x

readMem
    :: MonadState Memory m
    => m Int
readMem = do
    m@Mem{..} <- get
    M.findWithDefault 0 mPos mRegs <$ put (m { mPos = mPos + 1 })

peekMem
    :: MonadState Memory m
    => Natural -> m Int
peekMem i = gets $ M.findWithDefault 0 i . mRegs

useFuel :: (MonadState Memory m, MonadError String m) => m ()
useFuel = do
    u <- gets mFuel
    unless (u > 0) $ throwError "ran out of fuel"
    modify $ \m -> m { mFuel = u - 1 }

-- | Defered version of 'withInput', to allow for maximum 'laziness'.
withInputLazy
    :: (Traversable t, Applicative t, MonadState Memory m, MonadError String m)
    => Int      -- ^ mode int
    -> (t (m Int) -> m r)
    -> m (r, Mode)
withInputLazy mo f = do
    (lastMode, modes) <- case fillModes mo of
      Left  i -> throwError $ "bad mode: " ++ show i
      Right x -> pure x
    ba  <- gets mBase
    inp <- for modes $ \mode -> do
      a <- readMem
      pure $ case mode of
        Pos -> do
          x <- toNatural' a
          peekMem x
        Imm -> pure a
        Rel -> do
          x <- toNatural' (a + ba)
          peekMem x
    (, lastMode) <$> f inp

-- | Run a @t Int -> m r@ function by fetching an input container
-- @t Int@. This fetches everything in advance, so is 'strict'.
withInput
    :: (Traversable t, Applicative t, MonadState Memory m, MonadError String m)
    => Int      -- ^ mode int
    -> (t Int -> m r)
    -> m (r, Mode)
withInput mo f = withInputLazy mo ((f =<<) . sequenceA)


intMode :: Int -> Maybe Mode
intMode = \case 0 -> Just Pos
                1 -> Just Imm
                2 -> Just Rel
                _ -> Nothing

-- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- op int.
fillModes :: forall t. (Traversable t, Applicative t) => Int -> Either Int (Mode, t Mode)
fillModes i = do
    (lastMode, ms) <- traverse sequence $ mapAccumL go i (pure ())
    (,ms) <$> maybeToEither lastMode (intMode lastMode)
  where
    go j _ = (t, maybeToEither o $ intMode o)
      where
        (t,o) = j `divMod` 10

-- | Useful type to abstract over the actions of the different operations
data InstrRes = IRWrite Int         -- ^ write a value to location at
              | IRJump  Int         -- ^ jump to position
              | IRBase  Int         -- ^ set base
              | IRNop               -- ^ do nothing
              | IRHalt              -- ^ halt
  deriving Show

step
    :: forall m. (MonadError String m, MonadState Memory m)
    => Pipe Int Int Void m Bool
step = do
    useFuel
    (mo, x) <- (`divMod` 100) <$> readMem
    o  <- maybeToEither ("bad instr: " ++ show x) $ instr x
    (ir, lastMode) <- case o of
      Add -> withInput     mo $ \case V2 a b -> pure . IRWrite $ a + b
      Mul -> withInput     mo $ \case V2 a b -> pure . IRWrite $ a * b
      Get -> withInput     mo $ \case V0     -> IRWrite <$> awaitSurely
      Put -> withInput     mo $ \case V1 a   -> IRNop <$ yield a
      Jnz -> withInputLazy mo $ \case V2 a b -> a >>= \case 0 -> pure IRNop  ; _ -> IRJump <$> b
      Jez -> withInputLazy mo $ \case V2 a b -> a >>= \case 0 -> IRJump <$> b; _ -> pure IRNop
      Clt -> withInput     mo $ \case V2 a b -> pure . IRWrite $ if a <  b then 1 else 0
      Ceq -> withInput     mo $ \case V2 a b -> pure . IRWrite $ if a == b then 1 else 0
      ChB -> withInput     mo $ \case V1 a   -> pure $ IRBase a
      Hlt -> withInput     mo $ \case V0     -> pure IRHalt
    case ir of
      IRWrite y -> do
        c <- toNatural' =<< case lastMode of
          Pos -> peekMem =<< gets mPos
          Imm -> fromIntegral <$> gets mPos
          Rel -> (+) <$> (peekMem =<< gets mPos) <*> gets mBase
        _ <- readMem
        True <$ modify (\m -> m { mRegs = M.insert c y (mRegs m) })
      IRJump  y -> do
        z <- toNatural' y
        True <$ modify (\m -> m { mPos = z })
      IRBase  b ->
        True <$ modify (\m -> m { mBase = b + mBase m })
      IRNop     ->
        pure True
      IRHalt    ->
        pure False

stepForever
    :: (MonadState Memory m, MonadError String m)
    => Pipe Int Int Void m ()
stepForever = untilFalse step

untilFalse :: Monad m => m Bool -> m ()
untilFalse x = go
  where
    go = x >>= \case
      False -> pure ()
      True  -> go

runProg :: [Int] -> Memory -> Either String [Int]
runProg inp m = flip evalStateT m $
      runPipe $ (C.sourceList inp *> throwError "whoops")
             .| untilFalse step
             .| C.sinkList


maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure
