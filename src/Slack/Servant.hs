{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Slack.Servant (
    EventRequest(..)
  , UrlVerification(..)
  , EventCallback(..)
  , Event(..)
  , Message(..)
  -- , testObject
  , SlackServerApi(..)
  , SetTopic(..)
  , PostMessage(..)
  , slackServerClient
  ) where

import           Data.Char
import           Data.Proxy
import           Data.Text                     (Text)
import           Data.Time
import           Deriving.Aeson
import           Servant.API
import           Servant.API.Generic
import           Servant.Client.Core.RunClient
import           Servant.Client.Generic
import           Servant.Server
import           Web.Internal.FormUrlEncoded
import qualified Data.Aeson                    as A
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as T
import qualified Text.Casing                   as Case

data EventRequest =
    ERUrlVerification UrlVerification
  | EREventCallback EventCallback
  deriving (Generic, Show)

instance A.FromJSON EventRequest where
    parseJSON = A.withObject "EventRequest" $ \o -> do
      cb :: Text <- o A..: "type"
      case cb of
        "url_verification" -> ERUrlVerification <$> A.parseJSON (A.Object o)
        "event_callback" -> EREventCallback <$> A.parseJSON (A.Object o)
        _                -> fail $ "Invalid type: " <> T.unpack cb
      

data UrlVerification = UrlVerification
    { uvToken :: Text
    , uvChallenge :: Text
    }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "uv", CamelToSnake)] UrlVerification

data EventCallback = EventCallback
    { ecToken :: Text
    , ecTeamId :: Text
    , ecApiAppId :: Text
    , ecEvent :: A.Value
    , ecAuthedUsers :: Maybe [Text]
    , ecAuthorizations :: [Authorizations]
    , ecEventContext :: Text
    , ecEventId :: Text
    -- , ecEventTime :: UTCTime
    }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "ec", CamelToSnake)] EventCallback

-- testObject = A.Object . HM.fromList $
--   [("event_context", A.String "1-message-T037ZR4HR-C01DSU8EX6H")
--   ,("event",A.Object (HM.fromList [("ts",A.String "1604728044.002300"),("blocks",A.Array [A.Object (HM.fromList [("elements",A.Array [A.Object (HM.fromList [("elements",A.Array [A.Object (HM.fromList [("text",A.String "!help"),("type",A.String "text")])]),("type",A.String "rich_text_section")])]),("type",A.String "rich_text"),("block_id",A.String "p5B")])]),("client_msg_id",A.String "fe6dd9bd-adf9-4111-a3d0-cd9e89afac68"),("text",A.String "!help"),("channel",A.String "C01DSU8EX6H"),("user",A.String "U010SM1V9LL"),("event_ts",A.String "1604728044.002300"),("channel_type",A.String "channel"),("team",A.String "T037ZR4HR"),("type",A.String "message")]))
--   ,("team_id",A.String "T037ZR4HR")
--   ,("authorizations",A.Array [A.Object (HM.fromList [("team_id",A.String "T037ZR4HR"),("enterprise_id",A.Null),("is_enterprise_install",A.Bool False),("is_bot",A.Bool False),("user_id",A.String "U010SM1V9LL")])])
--   ,("token",A.String "aPkjS2kQDKjufj635C6H4Q4b")
--   ,("is_ext_shared_channel",A.Bool False)
--   ,("event_time",A.Number 1.604728044e9)
--   ,("api_app_id",A.String "A01DVJ2NJAJ")
--   ,("type",A.String "event_callback")
--   ,("event_id",A.String "Ev01E8JW55JP")
--   ]



data Event = Event
    { eType :: Text
    , eUser :: Maybe Text
    , eItem :: A.Value
    -- , eReaction :: Text
    -- , eItemUser :: Text
    , eEventTs :: UTCTime
    }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "e", CamelToSnake)] Event

data Authorizations = Authorizations
    { auEnterpriseId :: Maybe Text
    , auTeamId :: Maybe Text
    , auUserId :: Maybe Text
    , auIsBot :: Maybe Bool
    }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "au", CamelToSnake)] Authorizations

data Message = Message
    { mChannel :: Text
    , mUser :: Text
    , mText :: Text
    , mAppMention :: Bool
    -- , mTs :: UTCTime
    }
  deriving (Generic, Show)
  -- deriving (FromJSON, ToJSON)
  -- via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "m", CamelToSnake)] Message

instance FromJSON Message where
    parseJSON = A.withObject "Message" $ \o -> do
      tp         <- o A..:? "type"
      mChannel   <- o A..: "channel"
      mUser      <- o A..: "user"
      mText      <- o A..: "text"
      let mAppMention = tp == Just ("app_mention" :: Text)
      pure Message{..}

    -- {
    --     "blocks": [
    --         {
    --             "block_id": "xfU",
    --             "elements": [
    --                 {
    --                     "elements": [
    --                         {
    --                             "type": "user",
    --                             "user_id": "U01EEUR6ATT"
    --                         },
    --                         {
    --                             "text": " yolo",
    --                             "type": "text"
    --                         }
    --                     ],
    --                     "type": "rich_text_section"
    --                 }
    --             ],
    --             "type": "rich_text"
    --         }
    --     ],
    --     "channel": "C01DSU8EX6H",
    --     "client_msg_id": "677596a0-6c2b-4304-bd26-6b6c5b49467b",
    --     "event_ts": "1604729229.004900",
    --     "team": "T037ZR4HR",
    --     "text": "<@U01EEUR6ATT> yolo",
    --     "ts": "1604729229.004900",
    --     "type": "app_mention",
    --     "user": "U010SM1V9LL"
    -- }

-- { "ts":"1604728995.004700"
-- , "blocks":[{"elements":[{"elements":[{"text":"!hello","type":"text"}],"type":"rich_text_section"}],"type":"rich_text","block_id":"+1g0O"}]
-- , "client_msg_id":"fbd8f0fd-b12f-4d96-bfcd-c643e266cff0"
-- , "text":"!hello"
-- , "channel":"C01DSU8EX6H"
-- , "user":"U010SM1V9LL"
-- , "event_ts":"1604728995.004700"
-- , "channel_type":"channel"
-- , "team":"T037ZR4HR"
-- , "type":"message"
-- }


-- {"ts":"1604729229.004900","blocks":[{"elements":[{"elements":[{"type":"user","user_id":"U01EEUR6ATT"},{"text":" yolo","type":"text"}],"type":"rich_text_section"}],"type":"rich_text","block_id":"xfU"}],"client_msg_id":"677596a0-6c2b-4304-bd26-6b6c5b49467b","text":"<@U01EEUR6ATT> yolo","channel":"C01DSU8EX6H","user":"U010SM1V9LL","event_ts":"1604729229.004900","channel_type":"channel","team":"T037ZR4HR","type":"message"}
-- {"ts":"1604729229.004900","blocks":[{"elements":[{"elements":[{"type":"user","user_id":"U01EEUR6ATT"},{"text":" yolo","type":"text"}],"type":"rich_text_section"}],"type":"rich_text","block_id":"xfU"}],"client_msg_id":"677596a0-6c2b-4304-bd26-6b6c5b49467b","text":"<@U01EEUR6ATT> yolo","channel":"C01DSU8EX6H","user":"U010SM1V9LL","event_ts":"1604729229.004900","team":"T037ZR4HR","type":"app_mention"}

-- {
--     "blocks": [
--         {
--             "block_id": "xfU",
--             "elements": [
--                 {
--                     "elements": [
--                         {
--                             "type": "user",
--                             "user_id": "U01EEUR6ATT"
--                         },
--                         {
--                             "text": " yolo",
--                             "type": "text"
--                         }
--                     ],
--                     "type": "rich_text_section"
--                 }
--             ],
--             "type": "rich_text"
--         }
--     ],
--     "channel": "C01DSU8EX6H",
--     "channel_type": "channel",
--     "client_msg_id": "677596a0-6c2b-4304-bd26-6b6c5b49467b",
--     "event_ts": "1604729229.004900",
--     "team": "T037ZR4HR",
--     "text": "<@U01EEUR6ATT> yolo",
--     "ts": "1604729229.004900",
--     "type": "message",
--     "user": "U010SM1V9LL"
-- },
-- {
--     "blocks": [
--         {
--             "block_id": "xfU",
--             "elements": [
--                 {
--                     "elements": [
--                         {
--                             "type": "user",
--                             "user_id": "U01EEUR6ATT"
--                         },
--                         {
--                             "text": " yolo",
--                             "type": "text"
--                         }
--                     ],
--                     "type": "rich_text_section"
--                 }
--             ],
--             "type": "rich_text"
--         }
--     ],
--     "channel": "C01DSU8EX6H",
--     "client_msg_id": "677596a0-6c2b-4304-bd26-6b6c5b49467b",
--     "event_ts": "1604729229.004900",
--     "team": "T037ZR4HR",
--     "text": "<@U01EEUR6ATT> yolo",
--     "ts": "1604729229.004900",
--     "type": "app_mention",
--     "user": "U010SM1V9LL"
-- }

data SetTopic = SetTopic
    { stToken   :: Text
    , stChannel :: Text
    , stTopic   :: Text
    }
  deriving (Generic, Show)

instance ToForm SetTopic where
    toForm = genericToForm (defaultFormOptions
      { fieldLabelModifier = map toLower . Case.snake . drop 2
      })

data PostMessage = PostMessage
    { pmToken :: Text
    , pmChannel :: Text
    , pmText :: Text
    -- , pmAsUser :: Maybe Bool
    -- , pmAttachments :: Maybe A.Value
    -- , pmBlocks :: Maybe A.Value
    -- , pmIconEmoji :: Maybe Text
    }
  deriving (Generic, Show)

instance ToForm PostMessage where
    toForm = genericToForm (defaultFormOptions
      { fieldLabelModifier = map toLower . Case.snake . drop 2
      })


data SlackServerApi route = SlackServerApi
    { ssaSetTopic    :: route :- "api" :> "conversations.setTopic"
                              :> ReqBody '[FormUrlEncoded] SetTopic
                              :> Post '[JSON] A.Value
    , ssaPostMessage :: route :- "api" :> "chat.postMessage"
                              :> ReqBody '[FormUrlEncoded] PostMessage
                              :> Post '[JSON] A.Value
    }
  deriving Generic

slackServerApi :: Proxy (SlackServerApi AsApi)
slackServerApi = Proxy

slackServerClient :: RunClient m => SlackServerApi (AsClientT m)
slackServerClient = genericClient


       -- "santabot"
    -- :> ReqBody '[FormUrlEncoded] SlashData
    -- :> PostNoContent '[PlainText] NoContent
  -- :<|> "events"
    -- :> ReqBody '[JSON] Slack.EventRequest
    -- :> Post '[JSON] A.Value
-- https://slack.com/api/conversations.setTopic

