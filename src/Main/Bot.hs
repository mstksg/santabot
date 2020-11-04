{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main.Bot (
    masterBot
  , BotConf(..)
  ) where

import           Advent
import           Advent.Module.Intcode
import           Control.Monad.Reader
import           Data.Conduit.Lift
import           Data.Functor.Contravariant
import           Data.IORef
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Time.Format
import           GHC.Generics
import           Main.Commands
import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           Numeric.Natural
import           Santabot.Bot
import qualified Data.Aeson                 as A
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T
import qualified Data.Yaml                  as Y
import qualified Dhall                      as D
import qualified Language.Haskell.Printf    as P
import qualified Text.Casing                as Case


data BotConf = BotConf
    { bcName        :: T.Text           -- ^ name for instance
    , bcAuthor      :: T.Text           -- ^ author name
    , bcHome        :: String           -- ^ "home" channel
    , bcAlerts      :: String           -- ^ channel to send alerts to
    , bcSession     :: Maybe String     -- ^ AoC session key
    , bcLeaderboard :: Maybe Natural    -- ^ AoC Leaderboard Number
    , bcJoinCode    :: Maybe String     -- ^ AoC Leaderboard Join Code
    }
  deriving Generic

instance A.FromJSON BotConf where
    parseJSON = A.genericParseJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 2
      }

instance A.ToJSON BotConf where
    toJSON = A.genericToJSON A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '-' . drop 2
      }

instance D.FromDhall BotConf where
    autoWith _ = D.genericAutoWith $ D.defaultInterpretOptions
      { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack }

instance D.ToDhall BotConf where
    injectWith _ = D.genericToDhallWith $ D.defaultInterpretOptions
      { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack }

masterBot :: BotConf -> Manager -> IORef (Map Nick Paused) -> S.Set T.Text -> Bot IO ()
masterBot BotConf{..} mgr intcodeMap phrasebook = runReaderC phrasebook . mergeBots $
    [ commandBots $
        [ puzzleLink
        , puzzleThread
        , capTimeBot
        , nextPuzzle
        , intcodeBot mgr intcodeMap
        , simpleCommand "about" "Information about santabot" . addSantaPhrase . T.pack $
            [P.s|I'm a helper bot for %s and AoC util! Developed by %s, source at https://github.com/mstksg/santabot. All commands also work in private message.|]
            bcHome
            (T.unpack bcAuthor)
        , simpleCommand "time" "The current time on AoC servers" $ do
            t <- liftIO aocServerTime
            pure . T.pack $
              [P.s|The current AoC server time (EST) is %s|]
              (formatTime defaultTimeLocale "%H:%M:%S on %a, %-d %b %Y" t)
        ] ++ foldMap (:[]) (mkLeaderboard <$> bcLeaderboard <*> bcJoinCode)
    , alertBot bcAlerts challengeCountdown
    , alertBot bcAlerts eventCountdown
    , alertBot bcAlerts boardCapped
    , maybe idBot (alertBot bcAlerts) $
        privateCapped <$> bcSession <*> pure bcName <*> bcLeaderboard <*> pure bcJoinCode
    ]
  where
    mkLeaderboard lb jc = simpleCommand "leaderboard" (bcName <> " leaderboard") . (addSantaPhrase =<<) $ do
      Just y <- S.lookupMax <$> liftIO validYears
      pure . T.pack $
        [P.s|Join the %s Leaderboard! Code %d-%s, viewable at https://adventofcode.com/%04d/leaderboard/private/view/382266.|]
        (T.unpack bcName) lb jc y

