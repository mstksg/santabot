{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Santabot.Bot (
  Message (..),
  RespType (..),
  Event (..),
  Resp (..),
  Bot,
  Command (..),
  Alert (..),
  commandBot,
  commandBots,
  alertBot,
  risingEdgeAlert,
  mergeBots,
  simpleCommand,
  helpBot,
  intervals,
  idBot,
  Nick (..),
  lowerNick,
) where

import Advent
import Advent.Cache
import Conduit
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Char
import qualified Data.Conduit.Combinators as C
import Data.Function
import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time as Time
import qualified Data.Yaml as Y
import qualified Language.Haskell.Printf as P
import Numeric.Interval (Interval, (...))
import qualified Numeric.Interval as I
import System.Directory
import System.FilePath

data Message = M
  { mRoom :: String
  , mUser :: String
  , mBody :: Text
  }
  deriving (Show)

data Event
  = -- | time for AoC servers
    ETick LocalTime
  | EMsg Message
  deriving (Show)

data RespType
  = RTMessage
  | RTAction
  | RTNotice
  deriving (Show)

data Resp = R
  { rRoom :: String
  , rType :: RespType
  , rBody :: Text
  }
  deriving (Show)

type Bot = ConduitT Event Resp

mergeBots :: Monad m => [Bot m ()] -> Bot m ()
mergeBots = void . sequenceConduits

-- | The bot that does nothing
idBot :: Bot m ()
idBot = pure ()

data Command m = forall a. C
  { cName :: String
  , cHelp :: Text
  , cParse :: Message -> m (Either Text a)
  , cResp :: a -> m Text
  }

commandBot ::
  Monad m =>
  Command m ->
  Bot m ()
commandBot C{..} =
  C.concatMapM parseMe
    .| C.mapM displayMe
  where
    parseMe = \case
      EMsg m
        | Just (_, "", rest) <- T.commonPrefixes ("!" <> T.pack cName <> " ") (mBody m <> " ") ->
            Just . (mRoom m,) <$> cParse (m{mBody = T.strip rest})
      _ -> pure Nothing
    displayMe (room, r) = case r of
      Left e ->
        pure
          R
            { rRoom = room
            , rType = RTMessage
            , rBody = T.pack $ [P.s|%s: %s (!help %s for help)|] cName (T.unpack e) cName
            }
      Right b -> R room RTMessage <$> cResp b

helpBot ::
  Applicative m =>
  [Command m] ->
  Command m
helpBot cs =
  C
    { cName = "help"
    , cHelp = "Display list of commands and their usage instructions"
    , cParse = \M{..} ->
        pure $
          if mBody == ""
            then Right Nothing
            else case M.lookup mBody cMap of
              Nothing -> Left $ "Command not found: " <> mBody
              Just h -> Right $ Just (mBody, h)
    , cResp = pure . processHelp
    }
  where
    cMap =
      M.fromList [(T.pack cName, cHelp) | C{..} <- cs]
        <> M.singleton "help" "Display list of commands"
    processHelp = \case
      Nothing -> ("Available commands: " <>) . T.intercalate ", " $ M.keys cMap
      Just (cmd, msg) -> T.pack $ [P.s|%s: %s|] (T.unpack cmd) (T.unpack msg)

simpleCommand ::
  Applicative m =>
  -- | trigger
  String ->
  -- | help
  Text ->
  -- | response
  m Text ->
  Command m
simpleCommand nm hlp resp =
  C
    { cName = nm
    , cHelp = hlp
    , cParse = \_ -> pure $ Right ()
    , cResp = const resp
    }

-- | Like 'mergeBots' except adds help message
commandBots ::
  Monad m =>
  [Command m] ->
  Bot m ()
commandBots cs = mergeBots . map commandBot $ helpBot cs : cs

data Alert m = forall a. A
  { aTrigger :: Interval LocalTime -> m (Maybe a)
  , aResp :: a -> m (Bool, Text)
  -- ^ Bool is whether or not to NOTICE (otherwise, ACTION)
  }

alertBot ::
  MonadIO m =>
  -- | channel
  String ->
  Alert m ->
  Bot m ()
alertBot c A{..} =
  intervals
    .| C.concatMapM aTrigger
    .| C.mapM (fmap (\(rt, b) -> R c (toResp rt) b) . aResp)
  where
    toResp = \case
      False -> RTAction
      True -> RTNotice

intervals :: Monad m => ConduitT Event (Interval LocalTime) m ()
intervals =
  C.concatMap (\e -> [t | ETick t <- Just e] :: Maybe LocalTime)
    .| consecs
  where
    consecs = do
      x0 <- await
      mapM_ go x0
      where
        go x0 = do
          x1_ <- await
          forM_ x1_ $ \x1 -> do
            yield (x0 ... x1)
            go x1

data CapState
  = -- | file not even made yet
    CSEmpty
  | -- | file made but it is False
    CSNeg
  | -- | file made and it is True
    CSPos

-- | Generalized way to make an 'Alert' for a "rising edge" event.
risingEdgeAlert ::
  forall m a.
  MonadIO m =>
  -- | cache dir
  FilePath ->
  -- | cap log dir
  String ->
  -- | number of minutes between polls
  Int ->
  -- | notice? (true) or action (false)
  Bool ->
  -- | trigger with Just when "on" detected
  (Integer -> Advent.Day -> m (Maybe a)) ->
  -- | how to respond to /first/ 'Just'
  (Integer -> Advent.Day -> a -> m Text) ->
  Alert m
risingEdgeAlert cacheDir capLog delay noticeMe trigger response =
  A
    { aTrigger = risingEdge
    , aResp = fmap (noticeMe,) . uncurry sendEdge
    }
  where
    logDir = cacheDir </> capLog
    risingEdge i@(I.sup -> isup) = runMaybeT $ do
      liftIO $ createDirectoryIfMissing True logDir
      guard $ mm == 12
      guard $ withDelay `I.member` i
      d' <- maybe empty pure $ mkDay (fromIntegral dd)
      let logFP = logDir </> [P.s|%d-%02d|] yy (dayInt d') -<.> "yaml"
      liftIO (getCapState logFP) >>= \case
        CSPos -> empty
        CSEmpty -> liftIO (Y.encodeFile logFP False) *> empty
        CSNeg -> do
          trigRes <- MaybeT $ trigger yy d'
          pure (trigRes, (logFP, (yy, d')))
      where
        TimeOfDay hh uu _ = localTimeOfDay isup
        withDelay = isup{localTimeOfDay = TimeOfDay hh ((uu `div` delay) * delay) 0}
        d = localDay isup
        (yy, mm, dd) = toGregorian d
    sendEdge trigRes (logFP, (y, d)) = do
      liftIO $ Y.encodeFile logFP True
      response y d trigRes
    getCapState l =
      readFileMaybe l <&> \case
        Nothing -> CSEmpty
        Just x -> case Y.decodeEither' (T.encodeUtf8 x) of
          Left _ -> CSEmpty
          Right False -> CSNeg
          Right True -> CSPos

newtype Nick = Nick {unNick :: String}
  deriving (Show)

lowerNick :: Nick -> String
lowerNick (Nick s) = map go s
  where
    go '{' = '['
    go '}' = ']'
    go '|' = '\\'
    go c = toLower c

instance Eq Nick where
  (==) = (==) `on` lowerNick

instance Ord Nick where
  compare = compare `on` lowerNick
