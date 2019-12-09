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

type Parser = Parsec Void Text

data ICom = ICLaunch [Int] (Either (NESeq Int) U.URI)
          | ICPush   Nick (NonEmpty Int)
          -- | ICPipe   Nick Nick
          | ICClear
          | ICHelp
  deriving Show

parseCom :: Parser ICom
parseCom = asum
     [ try $ ICPush  <$> (lexeme "push" *> parseNick) <*> parseNE
     , try $ ICClear <$  lexeme "clear"
     , try $ ICHelp  <$  lexeme "help"
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

data Paused = Paused (Int -> VM) Memory

maxFuel :: Int
maxFuel = 100000

maxOut :: Int
maxOut = 100

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
          m    <- Mem maxFuel 0 0 . M.fromList . zip [0..] . toList <$> either pure (fetchRegs mgr) regsSpec
          curr <- liftIO $ readIORef v
          if Nick user `M.member` curr
            then pure $ [P.s|%s currently has a running thread. !intcode clear to delete.|] user
            else liftIO $ handleOut (Nick user) curr $ runStateT (feedPipe inps stepForever) m
        -- ICLaunch _ (Right link) -> pure . T.pack $
        --   [P.s|Loading %s ... jk, this isn't implemented yet|] (T.pack $ U.render link)
        ICPush nk (i :| is) -> fmap T.pack . liftIO $ do
          curr <- atomicModifyIORef v $ \mp -> (M.delete nk mp, mp)
          case nk `M.lookup` curr of
            Nothing              ->
              pure $ [P.s|No thread for %s found|] (unNick nk)
            Just (Paused next m) ->
              handleOut nk curr $ runStateT (feedPipe is (next i)) m
        ICClear -> liftIO . atomicModifyIORef v $ \mp ->
          let (Any found, mp') = M.alterF (const (Any True, Nothing)) (Nick user) mp
          in  (mp',) . T.pack $ if found
                then [P.s|Thread %s deleted|] user
                else [P.s|No thread for %s found|] user
        -- ICHelp  -> pure "Valid commands: <prog>; <inps> | <prog>; push <id> <inps>; pipe <id> <id>; clear; help"
        ICHelp  -> pure "Valid commands: <prog>; <inps> | <prog>; push <id> <inps>; clear; help"
    }
  where
    displayOutput (splitAt maxOut->(out, rest))
        | null out         = "<no output>"
        -- | all isPrint out' = [P.s|output: %s (%s)|] out' outarr
        | all inBounds out && all isPrint outStr && length out > 2 = [P.s|output: %s%s|] outStr truncString
        | otherwise        = [P.s|output: %s%s|] outarr truncString
      where
        outStr = map chr out
        inBounds c = c <= ord maxBound && c >= ord minBound
        outarr = intercalate "," (map show out)
        truncString
          | null rest = ""
          | otherwise = " (truncated)"

    handleOut nk curr = \case
      Left e                  -> pure $ [P.s|error: %s|] e
      Right ((outs, res), m') -> case res of
        Left next -> do
          writeIORef v $ M.insert nk (Paused next m') curr
          pure $
            [P.s|%s... awaiting input (!intcode push %s <inp> to continue)|]
            -- [P.s|%s... awaiting input (!intcode push %s <inp> to continue) (%d fuel remaining)|]
              (displayOutput outs)
              (unNick nk)
              -- (mFuel m')
        Right () -> pure $
            [P.s|%s; halt, @0 = %d; %d fuel unused|]
              (displayOutput outs)
              (M.findWithDefault 0 0 (mRegs m'))
              (mFuel m')

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
