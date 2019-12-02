{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Advent.Module.Intcode (
    intcodeBot
  , parseProg
  , runProg
  ) where

import           Data.Sequence           (Seq(..))
import           Santabot.Bot
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import qualified Data.Text.Read          as T
import qualified Language.Haskell.Printf as P

data Memory = Mem
    { mPos  :: !Int
    , mRegs :: !(Seq Int)
    }
  deriving Show

m2e :: e -> Maybe a -> Either e a
m2e e = maybe (Left e) Right

step :: Memory -> Either (Maybe String) Memory
step (Mem p r) = do
    x <- look p
    o <- case x of
      1  -> pure (+)
      2  -> pure (*)
      99 -> Left Nothing
      _  -> Left . Just $ [P.s|invalid opcode: %d|] x
    a <- look (p + 1)
    b <- look (p + 2)
    c <- look (p + 3)
    y <- look a
    z <- look b
    pure $ Mem (p + 4) (Seq.update c (o y z) r)
  where
    look i = m2e (Just $ [P.s|out of bounds: %d|] i) $
               Seq.lookup i r

runProg :: Memory -> Either String Int
runProg m = case step m of
    Left Nothing -> case Seq.lookup 0 (mRegs m) of
      Nothing -> Left "errored with: empty register"
      Just x  -> Right x
    Left (Just e) -> Left $ "errored with: " ++ e
    Right m'  -> runProg m'

parseProg :: T.Text -> Maybe Memory
parseProg = fmap (Mem 0 . Seq.fromList)
          . traverse (either (const Nothing) (Just . fst) . T.signed T.decimal . T.strip)
          . T.splitOn ","

intcodeBot :: Applicative m => Command m
intcodeBot = C
    { cName  = "intcode"
    , cHelp  = "Run arbitrary intcode (2019 Day 2) and try to crash jle`'s digial ocean droplet"
    , cParse = \m -> pure $ case parseProg (mBody m) of
                       Nothing -> Left "could not parse intcode program"
                       Just mm -> Right mm
    , cResp  = pure . T.pack . either id [P.s|halted with position 0 = %d|] . runProg
    }
