{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Advent.Module.Intcode (
    intcodeBot
  , parseIntcode
  , runIntcode
  ) where

import           Data.Bifunctor
import           Data.Sequence.NonEmpty  (NESeq(..))
import           Santabot.Bot
import qualified Data.List.NonEmpty      as NE
import qualified Data.Sequence.NonEmpty  as NESeq
import qualified Data.Text               as T
import qualified Data.Text.Read          as T
import qualified Language.Haskell.Printf as P

data Memory = Mem
    { mPos  :: !Int
    , mRegs :: !(NESeq Int)
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
    pure $ Mem (p + 4) (NESeq.update c (o y z) r)
  where
    look i = m2e (Just $ [P.s|out of bounds: %d|] i) $
               NESeq.lookup i r

runIntcode :: Memory -> Either String Int
runIntcode m = case step m of
    Left Nothing  -> Right $ NESeq.head (mRegs m)
    Left (Just e) -> Left $ "errored with: " ++ e
    Right m'  -> runIntcode m'

parseIntcode :: T.Text -> Either String Memory
parseIntcode str = do
    xs <- m2e "empty register" . NE.nonEmpty . T.splitOn "," $ str
    rs <- first (const "no parse integer")
        . traverse (fmap fst . T.signed T.decimal . T.strip)
        $ xs
    pure $ Mem 0 (NESeq.fromList rs)

intcodeBot :: Applicative m => Command m
intcodeBot = C
    { cName  = "intcode"
    , cHelp  = "Run arbitrary intcode (2019 Day 2) and try to crash jle`'s digial ocean droplet"
    , cParse = pure . first (("parse error: " <>) . T.pack) . parseIntcode . mBody
    , cResp  = pure . T.pack . either id [P.s|halted with position 0 = %d|] . runIntcode
    }
