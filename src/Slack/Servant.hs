{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Slack.Servant (
  EventRequest (..),
  UrlVerification (..),
  EventCallback (..),
  Event (..),
  Message (..),
  MessageType (..),
  -- , testObject
  SlackServerApi (..),
  SetTopic (..),
  PostMessage (..),
  slackServerClient,
  slackServerApi,
) where

import qualified Data.Aeson as A
import Data.Char
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Deriving.Aeson
import Servant.API
import Servant.Client.Core.RunClient
import Servant.Client.Generic
import qualified Text.Casing as Case
import Web.Internal.FormUrlEncoded

data EventRequest
  = ERUrlVerification UrlVerification
  | EREventCallback EventCallback
  deriving (Generic, Show)

instance A.FromJSON EventRequest where
  parseJSON = A.withObject "EventRequest" $ \o -> do
    cb :: Text <- o A..: "type"
    case cb of
      "url_verification" -> ERUrlVerification <$> A.parseJSON (A.Object o)
      "event_callback" -> EREventCallback <$> A.parseJSON (A.Object o)
      _ -> fail $ "Invalid type: " <> T.unpack cb

data UrlVerification = UrlVerification
  { uvToken :: Text
  , uvChallenge :: Text
  }
  deriving (Generic, Show)
  deriving
    (FromJSON, ToJSON)
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
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "ec", CamelToSnake)] EventCallback

data Event = Event
  { eType :: Text
  , eUser :: Maybe Text
  , eItem :: A.Value
  , -- , eReaction :: Text
    -- , eItemUser :: Text
    eEventTs :: UTCTime
  }
  deriving (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "e", CamelToSnake)] Event

data Authorizations = Authorizations
  { auEnterpriseId :: Maybe Text
  , auTeamId :: Maybe Text
  , auUserId :: Maybe Text
  , auIsBot :: Maybe Bool
  }
  deriving (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "au", CamelToSnake)] Authorizations

data MessageType
  = MTChannel
  | MTMention
  | MTIM
  deriving (Generic, Show, Eq, Ord)

data Message = Message
  { mChannel :: Text
  , mUser :: Text
  , mText :: Text
  , mType :: MessageType
  }
  deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = A.withObject "Message" $ \o -> do
    mChannel <- o A..: "channel"
    mUser <- o A..: "user"
    mText <- o A..: "text"
    isMention <- (== Just ("app_mention" :: Text)) <$> o A..:? "type"
    isIM <- (== Just ("im" :: Text)) <$> o A..:? "channel_type"
    let mType
          | isMention = MTMention
          | isIM = MTIM
          | otherwise = MTChannel
    pure Message{..}

data SetTopic = SetTopic
  { stToken :: Text
  , stChannel :: Text
  , stTopic :: Text
  }
  deriving (Generic, Show)

instance ToForm SetTopic where
  toForm =
    genericToForm
      ( defaultFormOptions
          { fieldLabelModifier = map toLower . Case.snake . drop 2
          }
      )

data PostMessage = PostMessage
  { pmToken :: Text
  , pmChannel :: Text
  , pmText :: Text
  }
  deriving (Generic, Show)

instance ToForm PostMessage where
  toForm =
    genericToForm
      ( defaultFormOptions
          { fieldLabelModifier = map toLower . Case.snake . drop 2
          }
      )

data SlackServerApi route = SlackServerApi
  { ssaSetTopic ::
      route
        :- "api"
          :> "conversations.setTopic"
          :> ReqBody '[FormUrlEncoded] SetTopic
          :> Post '[JSON] A.Value
  , ssaPostMessage ::
      route
        :- "api"
          :> "chat.postMessage"
          :> ReqBody '[FormUrlEncoded] PostMessage
          :> Post '[JSON] A.Value
  }
  deriving (Generic)

slackServerApi :: Proxy (SlackServerApi AsApi)
slackServerApi = Proxy

slackServerClient :: RunClient m => SlackServerApi (AsClientT m)
slackServerClient = genericClient
