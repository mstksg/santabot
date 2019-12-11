{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

module Advent.Module.Intcode (
    intcodeBot
  , parseCom
  , Paused
  ) where

import           Advent.Module.Intcode.VM
import           Conduit
import           Control.Applicative hiding         (many, some)
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Data.Bifunctor
import           Data.Char
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.List.NonEmpty                 (NonEmpty(..))
import           Data.Map                           (Map)
import           Data.Semigroup
import           Data.Sequence.NonEmpty             (NESeq)
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import           Network.HTTP.Conduit
import           Numeric.Natural
import           Santabot.Bot
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.Conduit                       as C
import qualified Data.Conduit.Combinators           as C
import qualified Data.Map                           as M
import qualified Data.Sequence.NonEmpty             as NESeq
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Language.Haskell.Printf            as P
import qualified Text.Megaparsec.Char.Lexer         as L
import qualified Text.URI                           as U

type VM_ = Pipe Int Int Void (Either String) (Maybe Natural, Memory)

type Parser = Parsec Void Text

data ICom = ICLaunch [Int] (Either (NESeq Int) U.URI)
          | ICPush   Nick (NonEmpty Int)
          | ICRefuel Nick
          -- | ICPipe   Nick Nick
          | ICClear
          | ICHelp
  deriving Show

parseCom :: Parser ICom
parseCom = asum
     [ try $ ICPush   <$> (lexeme "push" *> parseNick) <*> parseNE
     , try $ ICRefuel <$> (lexeme "refuel" *> parseNick)
     , try $ ICClear  <$  lexeme "clear"
     , try $ ICHelp   <$  lexeme "help"
     -- , try $ ICPipe  <$> (lexeme "pipe" *> parseNick) <*> parseNick
     , try $ ICLaunch <$> (parseList <* lexeme "|") <*> parseMem
     , try $ ICLaunch [] <$> parseMem
     ] <* eof
  where
    parseList :: Parser [Int]
    parseList = number `sepBy` optional (lexeme ",")
    parseMem :: Parser (Either (NESeq Int) U.URI)
    parseMem = (Left  <$> parseMemory    )
           <|> (Right <$> lexeme U.parser)
    parseNick :: Parser Nick
    parseNick = fmap Nick . lexeme $
      (:) <$> satisfy (`S.member` validNick1)
          <*> many (satisfy (`S.member` validNick))

lexeme      :: Parser a -> Parser a
lexeme      = L.lexeme space
number      :: Parser Int
number      = lexeme $ L.signed (pure ()) L.decimal
parseNE     :: Parser (NonEmpty Int)
parseNE     = number `NE.sepBy1` optional (lexeme ",")
parseMemory :: Parser (NESeq Int)
parseMemory = NESeq.fromList <$> parseNE

validNick1 :: Set Char
validNick1 = S.fromList $
     ['A'..'Z']
  ++ ['a'..'z']
  ++ "`|^_{}[]\\"

validNick :: Set Char
validNick = validNick1 <> S.fromList ('-':['0'..'9'])

data Paused = NeedInput (Int -> VM_)
            | NeedFuel  Memory
            -- Paused (Maybe (Int -> VM)) Memory


maxFuel :: Natural
maxFuel = 100000

maxOut :: Int
maxOut = 75

maxInput :: Int
maxInput = 100000

fetchRegs :: MonadResource m => Manager -> U.URI -> ExceptT String m (NESeq Int)
fetchRegs mgr u = do
    req <- maybe (throwError "bad uri") pure $ parseRequest (U.renderStr u)
    unless (host req `elem` validHosts) $
      throwError "unapproved link source (only raw github, raw gist, and raw pastebin)"
    resp <- http req mgr
    dat  <- runConduit $ responseBody resp
                    C..| C.decodeUtf8Lenient
                    C..| C.takeE maxInput
                    C..| C.fold
    either (const (throwError noParseE)) pure $
      runParser parseMemory (U.renderStr u) dat
  where
    noParseE = "could not parse data; may have been truncated. remember to use 'raw' link if available"
    validHosts = [ "raw.githubusercontent.com"
                 , "gist.githubusercontent.com"
                 , "pastebin.com"
                 , "dpaste.com"
                 ]

intcodeBot :: MonadUnliftIO m => Manager -> IORef (Map Nick Paused) -> Command m
intcodeBot mgr v = C
    { cName  = "intcode"
    , cHelp  = T.pack $ [P.s|Launch or interact with an intcode (2019 Days 2 & 5) process; max runtime %d instr. !intcode help for commands|]
                  maxFuel
    , cParse = \msg -> pure $ case runParser parseCom "" (mBody msg) of
          Left  _ -> Left "could not parse command"
          Right r -> Right (mUser msg, r)
    , cResp  = uncurry $ \user -> \case
        ICLaunch inps regsSpec -> fmap (either T.pack T.pack) . runResourceT . runExceptT $ do
          m    <- Mem 0 0 . M.fromList . zip [0..] . toList <$> either pure (fetchRegs mgr) regsSpec
          curr <- liftIO $ readIORef v
          if Nick user `M.member` curr
            then pure $ [P.s|%s currently has a running thread. !intcode clear to delete.|] user
            else liftIO $ handleOut (Nick user) curr $ feedPipe inps (runStateP m (stepN maxFuel))
        ICPush nk (i :| is) -> fmap T.pack . liftIO $ do
          curr <- readIORef v
          case nk `M.lookup` curr of
            Nothing              ->
              pure $ [P.s|No thread for %s found|] (unNick nk)
            Just (NeedFuel _) ->
              pure $ [P.s|Thread for %s out of fuel; use '!intcode refuel %s' to continue|]
                (unNick nk)
                (unNick nk)
            Just (NeedInput next) ->
              handleOut nk curr $ feedPipe is (next i)
        ICRefuel nk -> fmap T.pack . liftIO $ do
          curr <- readIORef v
          case nk `M.lookup` curr of
            Nothing              ->
              pure $ [P.s|No thread for %s found|] (unNick nk)
            Just (NeedFuel m) ->
              handleOut (Nick user) curr $ feedPipe [] (runStateP m (stepN maxFuel))
            Just (NeedInput _) ->
              pure $ [P.s|Thread for %s still has fuel; use '!intcode %s push <input>' to continue|]
                (unNick nk)
                (unNick nk)
        ICClear -> liftIO . atomicModifyIORef v $ \mp ->
          let (Any found, mp') = M.alterF (const (Any True, Nothing)) (Nick user) mp
          in  (mp',) . T.pack $ if found
                then [P.s|Thread %s deleted|] user
                else [P.s|No thread for %s found|] user
        -- ICHelp  -> pure "Valid commands: <prog>; <inps> | <prog>; push <id> <inps>; pipe <id> <id>; clear; help"
        ICHelp  -> pure "Valid commands: <prog>; <inps> | <prog>; push <id> <inps>; refuel <id>; clear; help"
    }
  where
    displayOutput (splitAt maxOut->(out, rest))
        | null out         = "<no output>"
        -- | all isPrint out' = [P.s|output: %s (%s)|] out' outarr
        | printAsString = [P.s|output: %s%s|] outStr truncString
        | otherwise        = [P.s|output: %s%s|] outarr truncString
      where
        printAsString = and
          [ all inBounds out
          , all isPrint outStr
          , length out > 2
          , all isAscii outStr
          ]
        outStr = map chr out
        inBounds c = c <= ord maxBound && c >= ord minBound
        (outarr, rest') = splitAt (maxOut * 2) $ intercalate "," (map show out)
        truncString
          | null rest && null rest' = ""
          | otherwise = " (truncated)"


    handleOut
        :: Nick
        -> Map Nick Paused
        -> Either String ([Int], Either (Int -> VM_) (Maybe Natural, Memory))
        -> IO String
    handleOut nk curr = \case
      Left e                  -> pure $ [P.s|error: %s|] e
      Right (outs, res) -> case res of
        Left next -> do
          writeIORef v $ M.insert nk (NeedInput next) curr
          pure $
            [P.s|%s... awaiting input (!intcode push %s <inp> to continue)|]
              (displayOutput outs)
              (unNick nk)
        Right (Just i, m') -> do
          writeIORef v $ M.delete nk curr
          pure $
            [P.s|%s; halt, @0 = %d; %d fuel unused|]
              (displayOutput outs)
              (M.findWithDefault 0 0 (mRegs m'))
              i
        Right (Nothing, m') -> do
          writeIORef v $ M.insert nk (NeedFuel m') curr
          pure $
            [P.s|%s; ran out of fuel ('!intcode refuel %s' to continue)|]
              (displayOutput outs)
              (unNick nk)

feedPipe
    :: Monad m
    => [i]
    -> Pipe i o u m a
    -> m ([o], Either (i -> Pipe i o u m a) a)
feedPipe xs = (fmap . second . first . fmap) fromRecPipe . feedPipe_ xs . toRecPipe

feedPipe_
    :: Monad m
    => [i]
    -> RecPipe i o u m a
    -> m ([o], Either (i -> RecPipe i o u m a) a)
feedPipe_ xs (FreeT p) = p >>= \case
    Pure y -> pure ([], Right y)
    Free (PAwaitF _ g) -> case xs of
      []   -> pure ([], Left g)
      y:ys -> feedPipe_ ys (g y)
    Free (PYieldF o q) -> first (o:) <$> feedPipe_ xs q


runStateP :: Monad m => s -> Pipe i o u (StateT s m) a -> Pipe i o u m (a, s)
runStateP s = fromRecPipe . runStateP_ s . toRecPipe

runStateP_ :: Monad m => s -> RecPipe i o u (StateT s m) a -> RecPipe i o u m (a, s)
runStateP_ s (FreeT p) = FreeT $ do
    (q, s') <- runStateT p s
    case q of
      Pure x -> pure $ Pure (x, s')
      Free l -> pure $ Free (fmap (runStateP_ s') l)
