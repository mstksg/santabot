{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

import           GHC.Generics
import           Santabot
import           Santabot.Bot
import           Santabot.Run
import qualified Data.Aeson   as A
import qualified Data.Yaml    as Y

data Conf = Conf
    { cChannels :: [String]
    , cNick     :: String
    , cPassword :: Maybe String
    }
  deriving Generic

instance A.FromJSON Conf where
    parseJSON = A.genericParseJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 1
      }

masterBot :: Bot IO ()
masterBot = mergeBots
  [ commandBot eventLink
  , alertBot challengeCountdown
  , alertBot eventCountdown
  -- , alertBot acknowledgeTick
  ]

main :: IO ()
main = do
    Conf{..} <- Y.decodeFileThrow "santabot-conf.yaml"
    launchIRC cChannels cNick cPassword 5000000 masterBot
