{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}

module Advent.Module.Intcode (
    intcodeBot
  , parseCom
  , Paused
  ) where

import           Advent.Module.Intcode.VM
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
import           Data.Void
import           Santabot.Bot
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.Map                           as M
import qualified Data.Sequence.NonEmpty             as NESeq
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Language.Haskell.Printf            as P
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void Text

data ICom = ICLaunch [Int] (Either (NESeq Int) String)      -- pastebin id
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
     , try $ ICLaunch <$> (parseList <* lexeme "|") <*> parseMemory
     , try $ ICLaunch [] <$> parseMemory
     ] <* eof
  where
    lexeme    = L.lexeme space
    number    :: Parser Int
    number    = lexeme $ L.signed (pure ()) L.decimal
    parseList :: Parser [Int]
    parseList = number `sepBy` optional (lexeme ",")
    parseNE :: Parser (NonEmpty Int)
    parseNE = number `NE.sepBy1` optional (lexeme ",")
    parseMemory :: Parser (Either (NESeq Int) String)
    parseMemory = (Left  <$> (NESeq.fromList <$> parseNE))
              <|> (Right <$> gistLink)
    gistLink :: Parser String
    gistLink = lexeme (some (satisfy (`S.member` validPastebin)))
    parseNick :: Parser Nick
    parseNick = fmap Nick . lexeme $
      (:) <$> satisfy (`S.member` validNick1)
          <*> many (satisfy (`S.member` validNick))

validNick1 :: Set Char
validNick1 = S.fromList $
     ['A'..'Z']
  ++ ['a'..'z']
  ++ "`|^_{}[]\\"

validNick :: Set Char
validNick = validNick1 <> S.fromList ('-':['0'..'9'])

validPastebin :: Set Char
validPastebin = S.fromList $
     ['A'..'Z']
  ++ ['a'..'z']
  ++ ['0'..'9']

data Paused = Paused (Int -> VM) Memory

maxFuel :: Int
maxFuel = 100000

intcodeBot :: MonadIO m => IORef (Map Nick Paused) -> Command m
intcodeBot v = C
    { cName  = "intcode"
    , cHelp  = T.pack $ [P.s|Launch or interact with an intcode (2019 Days 2 & 5) process; max runtime %d instr. !intcode help for commands|]
                  maxFuel
    , cParse = \msg -> pure $ case runParser parseCom "" (mBody msg) of
          Left  _ -> Left "could not parse command"
          Right r -> Right (mUser msg, r)
    , cResp  = uncurry $ \user -> \case
        ICLaunch inps (Left regs) ->
          let m = Mem maxFuel 0 regs
          in  fmap T.pack . liftIO $ do
                curr <- readIORef v
                if Nick user `M.member` curr
                  then pure $
                        [P.s|%s currently has a running thread. !intcode clear to delete.|] user
                  else handleOut user curr $ runStateT (feedPipe inps stepForever) m
        ICLaunch _ (Right link) -> pure . T.pack $
          [P.s|Loading pastebin %s ... jk, this isn't implemented yet|] link
        ICPush nk (i :| is) -> fmap T.pack . liftIO $ do
          curr <- atomicModifyIORef v $ \mp -> (M.delete nk mp, mp)
          case nk `M.lookup` curr of
            Nothing              ->
              pure $ [P.s|No thread for %s found|] (unNick nk)
            Just (Paused next m) ->
              handleOut user curr $ runStateT (feedPipe is (next i)) m
        ICClear -> liftIO . atomicModifyIORef v $ \mp ->
          let (Any found, mp') = M.alterF (const (Any True, Nothing)) (Nick user) mp
          in  (mp',) . T.pack $ if found
                then [P.s|Thread %s deleted|] user
                else [P.s|No thread for %s found|] user
        -- ICHelp  -> pure "Valid commands: <prog>; <inps> | <prog>; push <id> <inps>; pipe <id> <id>; clear; help"
        ICHelp  -> pure "Valid commands: <prog>; <inps> | <prog>; push <id> <inps>; clear; help"
    }
  where
    displayOutput out
        | null out         = "<no output>"
        | all isPrint out' = "output: " ++ out'
        | otherwise        = "output: " ++ intercalate "," (map show out)
      where
        out' = map chr out
    handleOut user curr = \case
      Left e                  -> pure $ [P.s|error: %s|] e
      Right ((outs, res), m') -> case res of
        Left next -> do
          writeIORef v $ M.insert (Nick user) (Paused next m') curr
          pure $
            [P.s|%s... awaiting input (!intcode push %s <inp> to continue) (%d fuel remaining)|]
              (displayOutput outs)
              user
              (mFuel m')
        Right () -> pure $
            [P.s|%s; halt, @0 = %d; %d fuel unused|]
              (displayOutput outs)
              (NESeq.head (mRegs m'))
              (mFuel m')

unrollPipe
    :: Monad m
    => Pipe i o u m a
    -> m (FreeF (PipeF i o u) a (Pipe i o u m a))
unrollPipe = (fmap . fmap) fromRecPipe . runFreeT . toRecPipe

feedPipe
    :: Monad m
    => [i]
    -> Pipe i o u m a
    -> m ([o], Either (i -> Pipe i o u m a) a)
feedPipe xs p = unrollPipe p >>= \case
    Pure y             -> pure ([], Right y)
    Free (PAwaitF _ f) -> case xs of
      []   -> pure ([], Left f)
      y:ys -> feedPipe ys (f y)
    Free (PYieldF o q) -> first (o:) <$> feedPipe xs q


