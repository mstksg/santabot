{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           GHC.Generics
import           Santabot
import           Santabot.Bot
import           Santabot.Run
import qualified Data.Aeson   as A
import qualified Data.Yaml    as Y

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

masterBot :: String -> Bot IO ()
masterBot alerts = mergeBots
  [ commandBots
      [ puzzleLink
      , nextPuzzle
      , simpleCommand "about" "Information about santabot" . pure $
          "Helper bot for ##adventofcode and advent of code utilities developed my jle`.  Source at https://github.com/mstksg/santabot"
      , simpleCommand "leaderboard" "IRC leaderboard" . pure $
          "Join the IRC Leaderboard! Code 382266-2c53e45d, viewable at https://adventofcode.com/2018/leaderboard/private/view/382266"
      ]
  , alertBot alerts challengeCountdown
  , alertBot alerts eventCountdown
  ]

main :: IO ()
main = do
    Conf{..} <- Y.decodeFileThrow "santabot-conf.yaml"
    launchIRC cChannels cNick cPassword (cTick * 1000000) (masterBot cAlerts)
