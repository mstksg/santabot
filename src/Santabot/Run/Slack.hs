{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Santabot.Run.Slack (
    launchSlack
  ) where

-- import qualified Web.Slack                    as Slack
-- import qualified Web.Slack.Message            as Slack
-- import qualified Web.Slack.WebAPI             as Slack
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
import           Data.Char
import           Data.Conduit hiding             (connect)
import           Data.Conduit.TQueue
import           Data.Proxy
import           Data.Text                       (Text)
import           Debug.Trace
import           Network.Wai.Handler.Warp        (run)
import           Santabot.Bot
import           Servant.API
import           Servant.API.Generic
import           Servant.Server
import           Web.Internal.FormUrlEncoded
import qualified Data.Aeson                      as A
import qualified Data.ByteString                 as BS
import qualified Data.Conduit.Combinators        as C
import qualified Data.IntMap                     as IM
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Language.Haskell.Printf         as P
import qualified Network.HTTP.Client             as HTTP
import qualified Text.Casing                     as Case


data SlashData = SlashData
    -- { sdToken :: Text
    -- , sdTeamId :: Text
    -- , sdEnterpriseId :: Text
    -- , sdEnterpriseName :: Text
    -- , sdChannelId :: Text
    -- , sdChannelName :: Text
    { sdUserName :: Text
    -- , sdUserId :: Text
    , sdCommand :: Text
    , sdText    :: Text
    , sdResponseUrl :: Text
    -- , sdTriggerId :: Text
    -- , sdApiAppId :: Text
    }
  deriving (Generic, Show)

instance FromForm SlashData where
    fromForm = genericFromForm (defaultFormOptions
      { fieldLabelModifier = map toLower . Case.snake . drop 2
      }) . traceShowId

type SlackApi =
       "santabot"
    :> ReqBody '[FormUrlEncoded] SlashData
    :> PostNoContent '[PlainText] NoContent

slackApi :: Proxy SlackApi
slackApi = Proxy

commandPrefix :: Text -> Text -> Text
commandPrefix cmd = case M.lookup cmd commandMap of
    Nothing  -> id
    Just pfx -> (pfx <>)
  where
    commandMap :: M.Map Text Text
    commandMap = M.fromList
      [ ("santabot", "")
      ]

slackServer
    :: TBMQueue Event
    -> TVar IM.Key
    -> Server SlackApi
slackServer eventQueue idCounter sd@SlashData{..} = liftIO $ do
    putStrLn $ "Received data: " <> show sd
    atomically $ do
      mid    <- stateTVar idCounter $ \i -> (i, i + 1)
      writeTBMQueue eventQueue $ EMsg
        M { mRoom = T.unpack sdResponseUrl  -- hm, we should look up first! otherwise private
          , mUser = T.unpack sdUserName      -- hm..
          , mBody = commandPrefix sdCommand sdText
          , mId   = mid
          }
    pure NoContent


launchSlack
    :: HTTP.Manager
    -> Int              -- ^ port
    -> Int              -- ^ tick delay (microseconds)
    -> Bot IO ()
    -> IO ()
launchSlack mgr port tick bot = do
    eventQueue <- atomically $ newTBMQueue 1000000
    idCounter  <- newTVarIO 0

    _ <- forkIO $
      run port $ serve slackApi (slackServer eventQueue idCounter)

    _ <- forkIO $ do
      threadDelay 5000000
      forever $ do
        threadDelay tick
        t <- aocServerTime
        atomically $ writeTBMQueue eventQueue (ETick t)

    putStrLn "Services launched..."

    runConduit $ sourceTBMQueue eventQueue
              .| bot
              .| C.iterM print
              .| C.mapM_ (responseHandler mgr)
              .| C.sinkNull

responseHandler
    :: HTTP.Manager
    -> Resp
    -> IO ()
responseHandler mgr R{..} = case HTTP.parseRequest rRoom of
    Nothing  -> putStrLn $ "Room not parsed: " <> rRoom
    Just req -> do
      putStrLn "Room parsed"
      let preq = req
            { HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS . A.encode $
                A.object
                  [ "text" A..= rBody
                  ]
            }
      HTTP.withResponse preq mgr $ \_ -> do
        putStrLn "got a resp"

    -- case rSource of
    --   Nothing  -> do
    --     ret <- runExceptT $
    --       Slack.chat_postMessage cfg (Slack.Id (T.pack rRoom)) rBody []
    --     case ret of
    --       Left e  -> do
    --         putStrLn "Error posting alert"
    --         putStrLn $ T.unpack rBody
    --         putStrLn $ T.unpack e
    --       Right _ -> pure ()
    --   Just mid -> void . forkIO . atomically $ do
    --     waiterMap <- readTVar waiters
    --     case IM.lookup mid waiterMap of
    --       Nothing -> retry
    --       Just w  -> putTMVar w (Slack.Id (T.pack rRoom), rBody)


-- eventHandler
--     :: TBMQueue Event
--     -> MVar ()
--     -> TVar (IM.IntMap (TMVar (Slack.ChannelId, T.Text)))
--     -> TVar IM.Key
--     -> Slack.Event
--     -> Slack.Slack () ()
-- eventHandler eventQueue started waiters idCounter = \case
--     Slack.Hello -> void . liftIO $ tryPutMVar started ()
--     Slack.Message cId (Slack.UserComment usr) body _ _ _ -> do
--       resp <- liftIO $ do
--         (waiter, mid) <- atomically $ do
--           mid    <- stateTVar idCounter $ \i -> (i, i + 1)
--           waiter <- newEmptyTMVar
--           modifyTVar' waiters $ IM.insert mid waiter
--           writeTBMQueue eventQueue $ EMsg
--             M { mRoom = T.unpack $ Slack._getId cId
--               , mUser = T.unpack $ Slack._getId usr
--               , mBody = body
--               , mId   = mid
--               }
--           pure (waiter, mid)
--         atomically $ do
--           resp <- takeTMVar waiter
--           modifyTVar' waiters $ IM.delete mid
--           pure resp
--       uncurry Slack.sendMessage resp
--     _ -> pure ()

-- responseHandler
--     :: Slack.SlackConfig
--     -> TVar (IM.IntMap (TMVar (Slack.ChannelId, T.Text)))
--     -> Resp
--     -> IO ()
-- responseHandler cfg waiters R{..} = do
--     case rSource of
--       Nothing  -> do
--         ret <- runExceptT $
--           Slack.chat_postMessage cfg (Slack.Id (T.pack rRoom)) rBody []
--         case ret of
--           Left e  -> do
--             putStrLn "Error posting alert"
--             putStrLn $ T.unpack rBody
--             putStrLn $ T.unpack e
--           Right _ -> pure ()
--       Just mid -> void . forkIO . atomically $ do
--         waiterMap <- readTVar waiters
--         case IM.lookup mid waiterMap of
--           Nothing -> retry
--           Just w  -> putTMVar w (Slack.Id (T.pack rRoom), rBody)

