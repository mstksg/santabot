{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MonadComprehensions       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module Santabot.Bot (
    Message(..)
  , Event(..)
  , Resp(..)
  , Bot
  , Command(..)
  , Alert(..)
  , commandBot
  , alertBot
  , mergeBots
  , aocTime
  ) where

import           Conduit
import           Data.Foldable
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

data Event = ETick LocalTime            -- ^ time for AoC servers
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
                .| C.mapM displayMe
  where
    parseMe = \case
      EMsg m
        | Just (_, "", rest) <- T.commonPrefixes ("!" <> cName <> " ") (mBody m)
        -> fmap (mRoom m,) <$> cParse (m { mBody = rest })
      _ -> pure Nothing
    displayMe (room, x) = R room <$> cResp x

data Alert m = forall a. A
    { aTrigger :: Interval LocalTime -> m (Maybe a)
    , aResp    :: a -> m Text
    }

alertBot
    :: MonadIO m
    => String       -- ^ channel
    -> Alert m
    -> Bot m ()
alertBot c A{..} = C.concatMap (\e -> [ t | ETick t <- Just e ] :: Maybe LocalTime)
                .| consecs
                .| C.concatMapM aTrigger
                .| C.mapM (fmap (R c) . aResp)
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

aocTime :: IO LocalTime
aocTime = utcToLocalTime (read "EST") <$> liftIO getCurrentTime
