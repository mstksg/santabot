{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Santabot.Run.Slack (
    launchSlack
  ) where

import           Advent
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Conduit hiding             (connect)
import           Data.Conduit.TQueue
import           Santabot.Bot
import qualified Data.ByteString                 as BS
import qualified Data.Conduit.Combinators        as C
import qualified Data.IntMap                     as IM
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Language.Haskell.Printf         as P
import qualified Web.Slack                       as Slack
import qualified Web.Slack.Message               as Slack
import qualified Web.Slack.WebAPI                as Slack

launchSlack
    :: Slack.SlackConfig
    -> Int              -- ^ tick delay (microseconds)
    -> Bot IO ()
    -> IO ()
launchSlack cfg tick bot = do

    eventQueue <- atomically $ newTBMQueue 1000000
    started    <- newEmptyMVar
    waiters    <- newTVarIO IM.empty
    idCounter  <- newTVarIO 0

    _ <- forkIO $
      Slack.runBot cfg (eventHandler eventQueue started waiters idCounter) ()

    _ <- forkIO $ do
      () <- takeMVar started
      threadDelay 5000000
      forever $ do
        threadDelay tick
        t <- aocServerTime
        atomically $ writeTBMQueue eventQueue (ETick t)

    runConduit $ sourceTBMQueue eventQueue
              .| bot
              .| C.mapM_ (responseHandler cfg waiters)
              .| C.sinkNull

eventHandler
    :: TBMQueue Event
    -> MVar ()
    -> TVar (IM.IntMap (TMVar (Slack.ChannelId, T.Text)))
    -> TVar IM.Key
    -> Slack.Event
    -> Slack.Slack () ()
eventHandler eventQueue started waiters idCounter = \case
    Slack.Hello -> void . liftIO $ tryPutMVar started ()
    Slack.Message cId (Slack.UserComment usr) body _ _ _ -> do
      resp <- liftIO $ do
        (waiter, mid) <- atomically $ do
          mid    <- stateTVar idCounter $ \i -> (i, i + 1)
          waiter <- newEmptyTMVar
          modifyTVar' waiters $ IM.insert mid waiter
          writeTBMQueue eventQueue $ EMsg
            M { mRoom = T.unpack $ Slack._getId cId
              , mUser = T.unpack $ Slack._getId usr
              , mBody = body
              , mId   = mid
              }
          pure (waiter, mid)
        atomically $ do
          resp <- takeTMVar waiter
          modifyTVar' waiters $ IM.delete mid
          pure resp
      uncurry Slack.sendMessage resp
    _ -> pure ()

responseHandler
    :: Slack.SlackConfig
    -> TVar (IM.IntMap (TMVar (Slack.ChannelId, T.Text)))
    -> Resp
    -> IO ()
responseHandler cfg waiters R{..} = do
    case rSource of
      Nothing  -> do
        ret <- runExceptT $
          Slack.chat_postMessage cfg (Slack.Id (T.pack rRoom)) rBody []
        case ret of
          Left e  -> do
            putStrLn "Error posting alert"
            putStrLn $ T.unpack rBody
            putStrLn $ T.unpack e
          Right _ -> pure ()
      Just mid -> void . forkIO . atomically $ do
        waiterMap <- readTVar waiters
        case IM.lookup mid waiterMap of
          Nothing -> retry
          Just w  -> putTMVar w (Slack.Id (T.pack rRoom), rBody)

