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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Void
import qualified Dhall as D
import qualified Dhall.Core as D
import Dhall.Src (Src)
import GHC.Generics
import qualified Language.Haskell.Printf as P
import Main.Commands
import Network.HTTP.Conduit
import Numeric.Natural
import Santabot.Bot
import qualified Text.Casing as Case

data BotConf = BotConf
  { bcName :: Text
  -- ^ name for instance
  , bcAlerts :: String
  -- ^ channel to send alerts to
  , bcCommandBots :: [CommandBot]
  , bcAlertBots :: [AlertBot]
  , bcPhrasebook :: S.Set Text
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
  | CBAbout Text
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

newtype EventCountdownCond = ECC {eccDhall :: D.Expr Src Void}

interpretECC :: EventCountdownCond -> Maybe (Natural -> Maybe Text)
interpretECC = D.rawInput (D.function D.inject D.auto) . eccDhall

instance D.FromDhall EventCountdownCond where
  autoWith _ = D.Decoder (pure . ECC) (pure $ D.Pi Nothing "_" D.Natural (D.App D.Optional D.Text))

instance D.ToDhall EventCountdownCond where
  injectWith _ = D.Encoder eccDhall (D.Pi Nothing "_" D.Natural (D.App D.Optional D.Text))

data AlertBot
  = ABChallengeCountdown (S.Set ChallengeEvent)
  | -- | seconds since next event
    ABEventCountdown EventCountdownCond
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
  FilePath ->
  Text ->
  Manager ->
  IORef (Map Nick Paused) ->
  CommandBot ->
  Command m
commandBotBot cacheDir name mgr intcodeMap = \case
  CBPuzzleLink -> puzzleLink
  CBPuzzleThread -> puzzleThread cacheDir
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
  FilePath ->
  Text ->
  AlertBot ->
  Alert m
alertBotBot cacheDir name = \case
  ABChallengeCountdown evts -> challengeCountdown evts
  ABEventCountdown ecc -> case interpretECC ecc of
    Nothing -> error "Bad Event Countdown Predicate, should not have typechecked"
    Just lim -> eventCountdown lim
  ABBoardCapped -> boardCapped cacheDir
  ABPrivateCapped PrivateCappedInfo{..} ->
    privateCapped
      cacheDir
      pciSession
      name
      (liLeaderboard pciLeaderboardInfo)
      pciCap

masterBot :: BotConf -> Manager -> IORef (Map Nick Paused) -> FilePath -> Bot IO ()
masterBot BotConf{..} mgr intcodeMap cacheDir =
  runReaderC bcPhrasebook . mergeBots $
    commandBots (commandBotBot cacheDir bcName mgr intcodeMap <$> bcCommandBots)
      : map (alertBot bcAlerts . alertBotBot cacheDir bcName) bcAlertBots
