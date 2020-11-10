{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Santabot.Run.Slack (
    launchSlack
  , TokenSet(..)
  ) where

import           Advent
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Conduit hiding             (connect)
import           Data.Conduit.TQueue
import           Data.Proxy
import           Data.Text                       (Text)
import           Network.Wai.Handler.Warp        (run)
import           Santabot.Bot
import           Servant.API
import           Servant.API.Generic
import           Servant.Client
import           Servant.Server
import           Web.Internal.FormUrlEncoded
import qualified Data.Aeson                      as A
import qualified Data.Conduit.Combinators        as C
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Dhall                           as D
import qualified Network.HTTP.Client             as HTTP
import qualified Slack.Servant                   as Slack
import qualified Text.Casing                     as Case


data SlashData = SlashData
    { sdToken :: Text
    , sdTeamId :: Text
    -- , sdEnterpriseId :: Text
    -- , sdEnterpriseName :: Text
    , sdChannelId :: Text
    , sdChannelName :: Text
    , sdUserName :: Text
    , sdUserId :: Text
    , sdCommand :: Text
    , sdText    :: Text
    , sdResponseUrl :: Text
    , sdTriggerId :: Text
    , sdApiAppId :: Text
    }
  deriving (Generic, Show)

instance FromForm SlashData where
    fromForm = genericFromForm (defaultFormOptions
      { fieldLabelModifier = map toLower . Case.snake . drop 2
      })

type SlackApi =
       "santabot"
    :> ReqBody '[FormUrlEncoded] SlashData
    :> PostNoContent '[PlainText] NoContent
  :<|> "events"
    :> ReqBody '[JSON] Slack.EventRequest
    :> Post '[JSON] A.Value

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
    :: Text                   -- ^ app user id, to strip if app_mention
    -> TBMQueue Event
    -> S.Set Text                     -- ^ watched channels
    -> Server SlackApi
slackServer appUser eventQueue channels = slashCommands :<|> eventCall
  where
    slashCommands sd@SlashData{..} = liftIO $ do
      putStrLn $ "Received data: " <> show sd
      atomically $
        writeTBMQueue eventQueue $ EMsg
          M { mRoom = T.unpack sdResponseUrl
            , mUser = T.unpack $ mentionStr sdUserName
            , mBody = commandPrefix sdCommand sdText
            }
      pure NoContent
    eventCall = \case
      Slack.ERUrlVerification Slack.UrlVerification{..} -> liftIO $ do
        putStrLn "got a challenge"
        pure $ A.object [ "challenge" A..= uvChallenge ]
      Slack.EREventCallback Slack.EventCallback{..} -> do
        case A.fromJSON ecEvent of
          A.Error   _                 -> pure ()
          A.Success msg@Slack.Message{..} ->
            let properChannel
                  | mChannel `S.member` channels = mType == Slack.MTChannel
                  | otherwise                    = mType /= Slack.MTChannel
            in  do when properChannel . liftIO $ do
                     print msg
                     atomically $
                       writeTBMQueue eventQueue $ EMsg
                         M { mRoom = T.unpack mChannel
                           , mUser = T.unpack $ mentionStr mUser
                           , mBody = maybe mText T.strip $
                               T.stripPrefix (mentionStr appUser) mText
                           }
        pure A.Null
    mentionStr nm = "<@" <> nm <> ">"


data TokenSet = TokenSet
    { tsBotToken :: T.Text
    , tsUserToken :: T.Text
    }
  deriving Generic

instance D.FromDhall TokenSet where
    autoWith _ = D.genericAutoWith $ D.defaultInterpretOptions
      { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack }

instance D.ToDhall TokenSet where
    injectWith _ = D.genericToDhallWith $ D.defaultInterpretOptions
      { D.fieldModifier = T.pack . Case.camel . drop 2 . T.unpack }

launchSlack
    :: HTTP.Manager
    -> Int              -- ^ port
    -> Int              -- ^ tick delay (microseconds)
    -> TokenSet         -- ^ oauth token
    -> Text             -- ^ app user id
    -> Maybe Text       -- ^ channel topic suffix
    -> S.Set Text       -- ^ watched channels
    -> Bot IO ()
    -> IO ()
launchSlack mgr port tick tokenSet appUser topic channels bot = do
    eventQueue <- atomically $ newTBMQueue 1000000

    _ <- forkIO $
      run port $ serve slackApi (slackServer appUser eventQueue channels)

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
              .| C.mapM_ (responseHandler mgr tokenSet topic)
              .| C.sinkNull

responseHandler
    :: HTTP.Manager
    -> TokenSet
    -> Maybe Text         -- ^ channel topic suffix
    -> Resp
    -> IO ()
responseHandler mgr TokenSet{..} topic R{..} = case rType of
    RTNotice -> do
      let setTopic = Slack.SetTopic
            { stToken   = tsUserToken
            , stChannel = T.pack rRoom
            , stTopic   = rBody <> maybe "" ("\n" <>) topic
            }
      void $ runClientM (ssaSetTopic setTopic) clientEnv
    RTMessage -> do
      let postMessage = Slack.PostMessage
            { pmToken   = tsBotToken
            , pmChannel = T.pack rRoom
            , pmText    = rBody
            }
      void $ runClientM (ssaPostMessage postMessage) clientEnv
    RTAction -> do
      let postMessage = Slack.PostMessage
            { pmToken   = tsBotToken
            , pmChannel = T.pack rRoom
            , pmText    = "/me " <> rBody
            }
      void $ runClientM (ssaPostMessage postMessage) clientEnv

  where
    Slack.SlackServerApi{..} = Slack.slackServerClient
    Just baseUrl = parseBaseUrl "https://slack.com"
    clientEnv = mkClientEnv mgr baseUrl

