{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}

module Elfbot (
  ) where


import           Advent
import           Advent.API
import           Conduit
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.Foldable
import           Data.Maybe
import           Data.Set                  (Set)
import           Data.Text                 (Text)
import           Data.Time
import           Servant.API
import           Servant.Client.Core
import           Servant.Links
import           Text.Megaparsec
import           Text.Read                 (readMaybe)
import qualified Data.Conduit.Combinators  as C
import qualified Data.Set                  as S
import qualified Data.Text                 as T

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

data Command m = forall a. C
    { cName  :: Text
    , cHelp  :: Text
    , cParse :: Message -> m (Maybe a)
    , cResp  :: a -> ConduitT a Text m ()
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
    displayMe (room, x) = C.map snd
                       .| cResp x
                       .| C.map (R room)

eventLink :: MonadIO m => Bot m ()
eventLink = commandBot $ C
    { cName  = "link"
    , cHelp  = "Get the link to a given event (!link 2017 23, !link 16).  Assumes current year if no year or invalid year given."
    , cParse = askLink
    , cResp  = displayLink
    }
  where
    askLink M{..} = runMaybeT $ do
        day  <- maybe empty pure . listToMaybe . mapMaybe mkDay $ w
        (yr,_,_) <- toGregorian . localDay . utcToLocalTime (read "EST")
          <$> liftIO getCurrentTime
        let year = fromMaybe yr . find (`S.member` years) $ w
        pure (year, day)
      where
        w = mapMaybe (readMaybe . T.unpack) . T.words . T.map clear $ mBody
        clear c
          | isDigit c = c
          | otherwise = ' '
        years = S.fromList [2015..2019]
    displayLink (yr, day) = yield $ T.pack u
      where
        rp :<|> _ = allLinks adventAPI yr
        rd :<|> _ = rp day
        u = showBaseUrl $ aocBase { baseUrlPath = show (linkURI rd) }
