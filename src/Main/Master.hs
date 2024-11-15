{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main.Master (
  masterBot,
  BotConf (..),
  AlertBot (..),
  CommandBot (..),
) where

import Advent
import Advent.Module.Intcode
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Conduit.Lift
import Data.IORef
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Format
import qualified Dhall as D
import GHC.Generics
import qualified Language.Haskell.Printf as P
import Main.Commands
import Network.HTTP.Conduit
import Numeric.Natural
import Santabot.Bot
import qualified Text.Casing as Case

data BotConf = BotConf
  { bcName :: T.Text
  -- ^ name for instance
  , bcAlerts :: String
  -- ^ channel to send alerts to
  , bcCommandBots :: [CommandBot]
  , bcAlertBots :: [AlertBot]
  , bcPhrasebook :: S.Set T.Text
  }
  deriving (Generic)

instance D.FromDhall BotConf where
  autoWith _ =
    D.genericAutoWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack
        }

instance D.ToDhall BotConf where
  injectWith _ =
    D.genericToDhallWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack
        }

data LeaderboardInfo = LeaderboardInfo
  { liLeaderboard :: Natural
  , liJoinCode :: Maybe String
  }
  deriving (Generic)

instance D.FromDhall LeaderboardInfo where
  autoWith _ =
    D.genericAutoWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack
        }

instance D.ToDhall LeaderboardInfo where
  injectWith _ =
    D.genericToDhallWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack
        }

data CommandBot
  = CBPuzzleLink
  | CBPuzzleThread
  | CBCapTimeBot
  | CBNextPuzzle
  | CBIntcode
  | CBAbout T.Text
  | CBTime
  | CBLeaderboard LeaderboardInfo
  deriving (Generic)

instance D.FromDhall CommandBot where
  autoWith _ =
    D.genericAutoWith $
      D.defaultInterpretOptions
        { D.constructorModifier = T.drop 2
        }

instance D.ToDhall CommandBot where
  injectWith _ =
    D.genericToDhallWith $
      D.defaultInterpretOptions
        { D.constructorModifier = T.drop 2
        }

data PrivateCappedInfo = PrivateCappedInfo
  { pciSession :: String
  , pciLeaderboardInfo :: LeaderboardInfo
  , pciCap :: Natural
  }
  deriving (Generic)

instance D.FromDhall PrivateCappedInfo where
  autoWith _ =
    D.genericAutoWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 3 . T.unpack
        }

instance D.ToDhall PrivateCappedInfo where
  injectWith _ =
    D.genericToDhallWith $
      D.defaultInterpretOptions
        { D.fieldModifier = T.pack . Case.camel . drop 3 . T.unpack
        }

data AlertBot
  = ABChallengeCountdown (S.Set ChallengeEvent)
  | -- | limit
    ABEventCountdown (Maybe Natural)
  | ABBoardCapped
  | ABPrivateCapped PrivateCappedInfo
  deriving (Generic)

instance D.FromDhall AlertBot where
  autoWith _ =
    D.genericAutoWith $
      D.defaultInterpretOptions
        { D.constructorModifier = T.drop 2
        }

instance D.ToDhall AlertBot where
  injectWith _ =
    D.genericToDhallWith $
      D.defaultInterpretOptions
        { D.constructorModifier = T.drop 2
        }

commandBotBot ::
  (MonadUnliftIO m, MonadFail m, MonadReader Phrasebook m) =>
  T.Text ->
  Manager ->
  IORef (Map Nick Paused) ->
  CommandBot ->
  Command m
commandBotBot name mgr intcodeMap = \case
  CBPuzzleLink -> puzzleLink
  CBPuzzleThread -> puzzleThread
  CBCapTimeBot -> capTimeBot
  CBNextPuzzle -> nextPuzzle
  CBIntcode -> intcodeBot mgr intcodeMap
  CBAbout t -> simpleCommand "about" "Information about santabot" . addSantaPhrase $ t
  CBTime -> simpleCommand "time" "The current time on AoC servers" $ do
    t <- liftIO aocServerTime
    pure . T.pack $
      [P.s|The current AoC server time (EST) is %s|]
        (formatTime defaultTimeLocale "%H:%M:%S on %a, %-d %b %Y" t)
  CBLeaderboard li ->
    simpleCommand "leaderboard" (name <> " leaderboard") $
      addSantaPhrase =<< mkLeaderboard li
  where
    mkLeaderboard LeaderboardInfo{..} = do
      Just y <- S.lookupMax <$> liftIO validYears
      pure . T.pack $ case liJoinCode of
        Just jc ->
          [P.s|Join the %s Leaderboard! Code %d-%s, viewable at https://adventofcode.com/%04d/leaderboard/private/view/%d|]
            (T.unpack name)
            liLeaderboard
            jc
            y
            liLeaderboard
        Nothing ->
          [P.s|View the %s Leaderboard! https://adventofcode.com/%04d/leaderboard/private/view/%d|]
            (T.unpack name)
            y
            liLeaderboard

alertBotBot ::
  (MonadUnliftIO m, MonadReader Phrasebook m) =>
  T.Text ->
  AlertBot ->
  Alert m
alertBotBot name = \case
  ABChallengeCountdown evts -> challengeCountdown evts
  ABEventCountdown lim -> eventCountdown lim
  ABBoardCapped -> boardCapped
  ABPrivateCapped PrivateCappedInfo{..} ->
    privateCapped
      pciSession
      name
      (liLeaderboard pciLeaderboardInfo)
      pciCap

masterBot :: BotConf -> Manager -> IORef (Map Nick Paused) -> Bot IO ()
masterBot BotConf{..} mgr intcodeMap =
  runReaderC bcPhrasebook . mergeBots $
    commandBots (commandBotBot bcName mgr intcodeMap <$> bcCommandBots)
      : map (alertBot bcAlerts . alertBotBot bcName) bcAlertBots
