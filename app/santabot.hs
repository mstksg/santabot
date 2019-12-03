{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

import           Advent
import           Advent.Module.Intcode
import           Control.Monad.Reader
import           Data.Conduit.Lift
import           Data.Time.Format
import           GHC.Generics
import           Santabot
import           Santabot.Bot
import           Santabot.Run
import qualified Data.Aeson              as A
import qualified Data.Set                as S
import qualified Data.Text               as T
import qualified Data.Yaml               as Y
import qualified Language.Haskell.Printf as P

data Conf = Conf
    { cChannels :: [String]
    , cAlerts   :: String
    , cTick     :: Int              -- ^ in seconds
    , cNick     :: String
    , cPassword :: Maybe String
    }
  deriving Generic

instance A.FromJSON Conf where
    parseJSON = A.genericParseJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 1
      }

masterBot :: Phrasebook -> String -> Bot IO ()
masterBot phrasebook alerts = runReaderC phrasebook . mergeBots $
  [ commandBots
      [ puzzleLink
      , puzzleThread
      , nextPuzzle
      , intcodeBot
      , simpleCommand "about" "Information about santabot" . addSantaPhrase $
          "I'm a helper bot for ##adventofcode and AoC util! Developed by jle`, source at https://github.com/mstksg/santabot. All commands also work in private message."
      , simpleCommand "leaderboard" "IRC leaderboard" . (addSantaPhrase =<<) $ do
          Just y <- S.lookupMax <$> liftIO validYears
          pure . T.pack $
            [P.s|Join the IRC Leaderboard! Code 382266-2c53e45d, viewable at https://adventofcode.com/%04d/leaderboard/private/view/382266.|]
            y
      , simpleCommand "time" "The current time on AoC servers" $ do
          t <- liftIO aocServerTime
          pure . T.pack $
            [P.s|The current AoC server time is %s|]
            (formatTime defaultTimeLocale rfc822DateFormat t)
      ]
  , alertBot alerts challengeCountdown
  , alertBot alerts eventCountdown
  , alertBot alerts boardCapped
  ]

main :: IO ()
main = do
    Conf{..}   <- Y.decodeFileThrow "santabot-conf.yaml"
    phrasebook <- S.fromList . map T.pack . lines <$> readFile "phrasebook.txt"
    launchIRC cChannels cNick cPassword (cTick * 1000000)
        (masterBot phrasebook cAlerts)
