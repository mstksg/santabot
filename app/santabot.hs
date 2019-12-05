{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import qualified Data.Text.Encoding      as T
import qualified Data.Text.IO            as T
import qualified Data.Yaml               as Y
import qualified Language.Haskell.Printf as P

data Conf = Conf
    { cChannels    :: [String]
    , cAlerts      :: String
    , cTick        :: Int              -- ^ in seconds
    , cNick        :: String
    , cPassword    :: Maybe String
    , cSession     :: Maybe String
    , cLeaderboard :: Maybe Integer
    , cJoinCode    :: Maybe String
    }
  deriving Generic

instance A.FromJSON Conf where
    parseJSON = A.genericParseJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 1
      }

instance A.ToJSON Conf where
    toJSON = A.genericToJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 1
      }

masterBot :: Conf -> S.Set T.Text -> Bot IO ()
masterBot Conf{..} phrasebook = runReaderC phrasebook . mergeBots $
    [ commandBots $
        [ puzzleLink
        , puzzleThread
        , nextPuzzle
        , intcodeBot
        , simpleCommand "about" "Information about santabot" . addSantaPhrase $
            "I'm a helper bot for ##adventofcode and AoC util! Developed by jle`, source at https://github.com/mstksg/santabot. All commands also work in private message."
        , simpleCommand "time" "The current time on AoC servers" $ do
            t <- liftIO aocServerTime
            pure . T.pack $
              [P.s|The current AoC server time (EST) is %s|]
              (formatTime defaultTimeLocale "%H:%M:%S, on %a, %d %b %Y" t)
        ] ++ foldMap (:[]) (mkLeaderboard <$> cLeaderboard <*> cJoinCode)
    , alertBot cAlerts challengeCountdown
    , alertBot cAlerts eventCountdown
    , alertBot cAlerts boardCapped
    , maybe idBot (alertBot cAlerts) $
        privateCapped <$> cSession <*> cLeaderboard <*> pure cJoinCode
    ]
  where
    mkLeaderboard lb jc = simpleCommand "leaderboard" "IRC leaderboard" . (addSantaPhrase =<<) $ do
      Just y <- S.lookupMax <$> liftIO validYears
      pure . T.pack $
        [P.s|Join the IRC Leaderboard! Code %d-%s, viewable at https://adventofcode.com/%04d/leaderboard/private/view/382266.|]
        lb jc y

main :: IO ()
main = do
    c@Conf{..} <- Y.decodeFileThrow "santabot-conf.yaml"
    T.putStrLn . T.decodeUtf8 . Y.encode $ c
    phrasebook <- S.fromList . map T.pack . lines <$> readFile "phrasebook.txt"
    launchIRC cChannels cNick cPassword (cTick * 1000000)
        (masterBot c phrasebook)
