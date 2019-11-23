{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module Elfbot.Bot (
    Message(..)
  , Event(..)
  , Resp(..)
  , Bot
  , Command(..)
  , Alert(..)
  , commandBot
  , alertBot
  , mergeBots
  ) where

import           Conduit
import           Data.Functor
import           Data.Text                (Text)
import           Data.Time                as Time
import           Numeric.Interval         (Interval, (...))
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T

data Message = M { mRoom :: String
                 , mUser :: String
                 , mBody :: Text
                 }
  deriving Show

data Event = ETick
           | EMsg  Message
  deriving Show

data Resp = R { rRoom :: String
              , rBody :: Text
              }
  deriving Show

type Bot = ConduitT Event Resp

mergeBots :: Monad m => [Bot m ()] -> Bot m ()
mergeBots = void . sequenceConduits

data Command m = forall a. C
    { cName  :: Text
    , cHelp  :: Text
    , cParse :: Message -> m (Maybe a)
    , cResp  :: a -> m Text
    }

commandBot
    :: Monad m
    => Command m
    -> Bot m ()
commandBot C{..} = C.concatMapM parseMe
                .| awaitForever displayMe
  where
    parseMe = \case
      EMsg m
        | Just (_, "", rest) <- T.commonPrefixes ("!" <> cName <> " ") (mBody m)
        -> fmap (mRoom m,) <$> cParse (m { mBody = rest })
      _ -> pure Nothing
    displayMe (room, x) = yield . R room =<< lift (cResp x)

data Alert m = forall a. A
    { aTrigger :: Interval LocalTime -> m (Maybe a)
    , aResp    :: a -> m Resp
    }

alertBot
    :: MonadIO m
    => Alert m
    -> Bot m ()
alertBot A{..} = do
    t0 <- utcToLocalTime (read "EST") <$> liftIO getCurrentTime
    go t0
      .| C.concatMapM aTrigger
      .| awaitForever (\x -> lift (aResp x) >>= yield)
  where
    go t0 = await >>= \_ -> do
      t1 <- utcToLocalTime (read "EST") <$> liftIO getCurrentTime
      yield (t0 ... t1)
      go t1

