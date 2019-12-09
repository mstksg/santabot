{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}


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
import           Data.IntMap               (IntMap)
import           Data.Map                  (Map)
import           Data.Traversable
import           Data.Void
import           Linear
import qualified Data.Conduino.Combinators as C
import qualified Data.IntMap               as IM
import qualified Data.Map                  as M

type VM = Pipe Int Int Void (StateT Memory (Either String)) ()

data Memory = Mem
    { mFuel :: Int
    , mPos  :: Int
    , mBase :: Int
    , mRegs :: IntMap Int
    }
  deriving Show

data Mode = Pos | Imm | Rel
  deriving (Eq, Ord, Enum, Show)

data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | ChB | Hlt
  deriving (Eq, Ord, Enum, Show)

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add ..]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

readMem
    :: MonadState Memory m
    => m Int
readMem = do
    m@Mem{..} <- get
    IM.findWithDefault 0 mPos mRegs <$ put (m { mPos = mPos + 1 })

peekMem
    :: MonadState Memory m
    => Int -> m Int
peekMem i = gets $ IM.findWithDefault 0 i . mRegs

useFuel :: (MonadState Memory m, MonadError String m) => m ()
useFuel = do
    u <- gets mFuel
    unless (u > 0) $ throwError "ran out of fuel"
    modify $ \m -> m { mFuel = u - 1 }

-- | Run a @t Int -> m r@ function by fetching an input container
-- @t Int@.
withInput
    :: (Traversable t, Applicative t, MonadState Memory m, MonadError String m)
    => Int      -- ^ mode int
    -> (t Int -> m r)
    -> m (r, Mode)
withInput mo f = do
    (lastMode, modes) <- case fillModes mo of
      Left  i -> throwError $ "bad mode: " ++ show i
      Right x -> pure x
    ba  <- gets mBase
    inp <- for modes $ \mode -> do
      a <- readMem
      case mode of
        Pos -> peekMem a
        Imm -> pure a
        Rel -> peekMem (a + ba)
    (, lastMode) <$> f inp

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
data InstrRes = IRWrite Int       -- ^ write a value to location at
              | IRJump  Int         -- ^ jump to position
              | IRBase  Int         -- ^ set base
              | IRNop               -- ^ do nothing
              | IRHalt              -- ^ halt
  deriving Show

step
    :: (MonadError String m, MonadState Memory m)
    => Pipe Int Int Void m Bool
step = do
    useFuel
    (mo, x) <- (`divMod` 100) <$> readMem
    o  <- maybeToEither ("bad instr: " ++ show x) $ instr x
    (ir, lastMode) <- case o of
      Add -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a + b
      Mul -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a * b
      Get -> withInput mo $ \case V0      -> IRWrite <$> awaitSurely
      Put -> withInput mo $ \case V1 a    -> IRNop <$ yield a
      Jnz -> withInput mo $ \case V2 a b  -> pure $ if a /= 0 then IRJump b else IRNop
      Jez -> withInput mo $ \case V2 a b  -> pure $ if a == 0 then IRJump b else IRNop
      Clt -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a <  b then 1 else 0
      Ceq -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a == b then 1 else 0
      ChB -> withInput mo $ \case V1 a    -> pure $ IRBase a
      Hlt -> withInput mo $ \case V0      -> pure IRHalt
    case ir of
      IRWrite y -> do
        c <- case lastMode of
               Pos -> peekMem =<< gets mPos
               Imm -> gets mPos
               Rel -> (+) <$> (peekMem =<< gets mPos) <*> gets mBase
        _ <- readMem
        True <$ modify (\m -> m { mRegs = IM.insert c y (mRegs m) })
      IRJump  z ->
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

-- stepForeverAndDie
--     :: MonadError String m
--     => Memory
--     -> Pipe Int Int Void m Void
-- stepForeverAndDie m = stepForever m *> throwError "no more input to give"

-- untilHalt
--     :: Monad m
--     => Pipe i o u (ExceptT String m) a
--     -> Pipe i o u m                  ()
-- untilHalt = void . runExceptP

-- parseMem :: String -> Maybe Memory
-- parseMem = fmap (Mem 0 0 . IM.fromList . zip [0..])
--          . traverse readMaybe
--          . splitOn ","

untilFalse :: Monad m => m Bool -> m ()
untilFalse x = go
  where
    go = x >>= \case
      False -> pure ()
      True  -> go

-- yieldAndDie :: MonadError String m => o -> Pipe i o u m a
-- yieldAndDie i = yield i *> throwError "that's all you get"

-- yieldAndPass :: o -> Pipe o o u m u
-- yieldAndPass i = yield i *> C.map id



-- type VM = Pipe Int Int Void (StateT Memory (Either String)) ()

-- data Memory = Mem
--     { mFuel :: Int
--     , mPos  :: Int
--     , mRegs :: NESeq Int
--     }
--   deriving Show

-- data Mode = Pos | Imm
--   deriving (Eq, Ord, Enum, Show)

-- data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
--   deriving (Eq, Ord, Enum, Show)

-- instrMap :: Map Int Instr
-- instrMap = M.fromList $
--     (99, Hlt) : zip [1 ..] [Add ..]

-- instr :: Int -> Maybe Instr
-- instr = (`M.lookup` instrMap)

-- readMem
--     :: (MonadState Memory m, MonadError String m)
--     => m Int
-- readMem = do
--     Mem u p r <- get
--     case NESeq.lookup p r of
--       Nothing -> throwError $ "bad index under current position: " ++ show p
--       Just x  -> x <$ put (Mem u (p + 1) r)

-- peekMem
--     :: (MonadState Memory m, MonadError String m)
--     => Int -> m Int
-- peekMem i = do
--     Mem _ _ r <- get
--     case NESeq.lookup i r of
--       Nothing -> throwError $ "bad index for peek: " ++ show i
--       Just x  -> pure x

-- -- | Run a @t Int -> m r@ function by getting fetching an input container
-- -- @t Int@.
-- withInput
--     :: (Traversable t, Applicative t, MonadState Memory m, MonadError String m)
--     => Int      -- ^ mode int
--     -> (t Int -> m r)
--     -> m r
-- withInput mo f = do
--     mos <- either (throwError . ("bad mode: " <>) . show) pure $ fillModes mo
--     inp <- for mos $ \mode -> do
--       a <- readMem
--       case mode of
--         Pos -> peekMem a
--         Imm  -> pure a
--     f inp

-- -- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- -- op int.
-- fillModes :: (Traversable t, Applicative t) => Int -> Either Int (t Mode)
-- fillModes i = sequence . snd $ mapAccumL go i (pure ())
--   where
--     go j _ = (t, case o of 0 -> Right Pos; 1 -> Right Imm; k -> Left k)
--       where
--         (t,o) = j `divMod` 10

-- -- | Useful type to abstract over the actions of the different operations
-- data InstrRes = IRWrite Int         -- ^ write a value
--               | IRJump  Int         -- ^ jump to position
--               | IRNop               -- ^ do nothing
--               | IRHalt              -- ^ halt
--   deriving Show

-- step
--     :: (MonadError String m, MonadState Memory m)
--     => Pipe Int Int Void m Bool
-- step = do
--     useFuel
--     (mo, x) <- (`divMod` 100) <$> readMem
--     o  <- maybeToEither ("bad instr: " ++ show x) $ instr x
--     ir <- case o of
--       Add -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a + b
--       Mul -> withInput mo $ \case V2 a b  -> pure . IRWrite $ a * b
--       Get -> IRWrite <$> awaitSurely
--       Put -> withInput mo $ \case V1 a    -> IRNop <$ yield a
--       Jnz -> withInput mo $ \case V2 a b  -> pure $ if a /= 0 then IRJump b else IRNop
--       Jez -> withInput mo $ \case V2 a b  -> pure $ if a == 0 then IRJump b else IRNop
--       Clt -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a <  b then 1 else 0
--       Ceq -> withInput mo $ \case V2 a b  -> pure . IRWrite $ if a == b then 1 else 0
--       Hlt                                 -> pure IRHalt
--     case ir of
--       IRWrite y -> do
--         c <- readMem
--         Mem u p r <- get
--         unless (c < NESeq.length r) $ throwError $ "Update out of range: " ++ show c
--         True <$ put (Mem u p (NESeq.update c y r))
--       IRJump  z ->
--         True <$ modify (\(Mem u _ r) -> Mem u z r)
--       IRNop     ->
--         pure True
--       IRHalt    ->
--         pure False

-- stepForever
--     :: (MonadState Memory m, MonadError String m)
--     => Pipe Int Int Void m ()
-- stepForever = untilFalse step

-- untilFalse :: Monad m => m Bool -> m ()
-- untilFalse x = go
--   where
--     go = x >>= \case
--       False -> pure ()
--       True  -> go


runProg :: [Int] -> Memory -> Either String [Int]
runProg inp m = flip evalStateT m $
      runPipe $ (C.sourceList inp *> throwError "whoops")
             .| untilFalse step
             .| C.sinkList


-- data Memory = Mem
--     { mPos  :: !Int
--     , mRegs :: !(NESeq Int)
--     }
--   deriving Show

maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

-- data Mode = Pos | Imm
--   deriving (Eq, Ord, Enum, Show)

-- data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
--   deriving (Eq, Ord, Enum, Show)

-- instrMap :: Map Int Instr
-- instrMap = M.fromList $
--     (99, Hlt) : zip [1 ..] [Add ..]

-- instr :: Int -> Maybe Instr
-- instr = (`M.lookup` instrMap) . (`mod` 100)

-- data InstrOp m = forall t. (Traversable t, Applicative t) => InstrOp (t Int -> Pipe Int Int Void m InstrRes)

-- data InstrRes = IRWrite Int
--               | IRJump  Int
--               | IRNop
--               | IRHalt
--   deriving Show

-- instrOp
--     :: MonadError String m
--     => Instr
--     -> InstrOp m
-- instrOp = \case
--     Add -> InstrOp $ \case V2 x y -> pure . IRWrite $ x + y
--     Mul -> InstrOp $ \case V2 x y -> pure . IRWrite $ x * y
--     Get -> InstrOp $ \case V0     -> await >>= \case
--                              Nothing -> throwError "no input";
--                              Just x  -> pure $ IRWrite x
--     Put -> InstrOp $ \case V1 x   -> IRNop <$ yield x
--     Jnz -> InstrOp $ \case V2 x y -> pure $ if x /= 0 then IRJump y else IRNop
--     Jez -> InstrOp $ \case V2 x y -> pure $ if x == 0 then IRJump y else IRNop
--     Clt -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x <  y then 1 else 0
--     Ceq -> InstrOp $ \case V2 x y -> pure . IRWrite $ if x == y then 1 else 0
--     Hlt -> InstrOp $ \case V0     -> pure IRHalt

-- runInstrOp
--     :: (Traversable t, MonadError String m)
--     => Memory
--     -> t Mode
--     -> (t Int -> m InstrRes)
--     -> m (InstrRes, Int)
-- runInstrOp (Mem p r) modes f = do
--     (inp, p') <- flip runStateT (p + 1) . for modes $ \m -> do
--       q <- tick
--       a <- m2e "bad index" $ NESeq.lookup q r
--       case m of
--         Pos -> m2e "bad index" $ NESeq.lookup a r
--         Imm -> pure a
--     (,p') <$> f inp
--   where
--     tick = state $ \i -> (i, i + 1)

-- -- | Magically fills a fixed-shape 'Applicative' with each mode from a mode
-- -- op int.
-- fillModes :: (Traversable t, Applicative t) => Int -> t Mode
-- fillModes i = snd $ mapAccumL go i (pure ())
--   where
--     go j _ = (t, case o of 0 -> Pos; _ -> Imm)
--       where
--         (t,o) = j `divMod` 10

-- step
--     :: MonadError String m
--     => Memory
--     -> Pipe Int Int Void m (Maybe Memory)
-- step m@(Mem p r) = do
--     x <- m2e "bad index" $ NESeq.lookup p r
--     o <- m2e "bad instr" $ instr x
--     case instrOp o of
--       InstrOp f -> do
--         (ir, q) <- runInstrOp m (fillModes (x `div` 100)) f
--         case ir of
--           IRWrite y -> do
--             c <- m2e "bad index" $ NESeq.lookup q r
--             pure . Just $ Mem (q + 1) (NESeq.update c y r)
--           IRJump  z ->
--             pure . Just $ Mem z r
--           IRNop     ->
--             pure . Just $ Mem q r
--           IRHalt    -> pure Nothing

-- stepForever :: MonadError String m => Memory -> Pipe Int Int Void m Memory
-- stepForever = loopMaybeM step

-- loopMaybeM
--     :: Monad m
--     => (a -> m (Maybe a))
--     -> a
--     -> m a
-- loopMaybeM f = go
--   where
--     go !x = f x >>= \case
--       Nothing -> pure x
--       Just !y -> go y

