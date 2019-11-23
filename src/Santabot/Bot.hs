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
  , commandBots
  , alertBot
  , mergeBots
  , simpleCommand
  , helpBot
  , aocTime
  ) where

import           Conduit
import           Data.Foldable
import           Data.Functor
import           Data.Text                (Text)
import           Data.Time                as Time
import           Numeric.Interval         (Interval, (...))
import           Text.Printf
import qualified Data.Conduit.Combinators as C
import qualified Data.Map                 as M
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
    , cParse :: Message -> m (Either Text a)
    , cResp  :: a -> m Text
    }

commandBot
    :: Monad m
    => Command m
    -> Bot m ()
commandBot C{..} = C.concatMapM parseMe
                .| C.mapM (uncurry displayMe)
  where
    parseMe = \case
      EMsg m
        | Just (_, "", rest) <- T.commonPrefixes ("!" <> cName <> " ") (mBody m <> " ")
        -> Just . (mRoom m,) <$> cParse (m { mBody = T.strip rest })
      _ -> pure Nothing
    displayMe room = \case
      Left  e -> pure $ R room $ cName <> ": " <> e
      Right r -> R room <$> cResp r

helpBot
    :: Applicative m
    => [Command m]
    -> Command m
helpBot cs = C
    { cName  = "help"
    , cHelp  = "Display list of commands and their usage instructions"
    , cParse = \M{..} -> pure $
        if mBody == ""
          then Right Nothing
          else case M.lookup mBody cMap of
                 Nothing -> Left  $ "Command not found: " <> mBody
                 Just h  -> Right $ Just (mBody, h)
    , cResp  = pure . processHelp
    }
  where
    cMap = M.fromList [ (cName, cHelp) | C{..} <- cs ]
        <> M.singleton "help" "Display list of commands"
    processHelp = \case
      Nothing         -> ("Available commands: " <>) . T.intercalate ", " $ M.keys cMap
      Just (cmd, msg) -> T.pack $ printf "%s: %s" (T.unpack cmd) (T.unpack msg)

simpleCommand
    :: Applicative m
    => Text             -- ^ trigger
    -> Text             -- ^ help
    -> m Text           -- ^ response
    -> Command m
simpleCommand nm hlp resp = C
    { cName  = nm
    , cHelp  = hlp
    , cParse = \_ -> pure $ Right ()
    , cResp  = const resp
    }

-- | Like 'mergeBots' except adds help message
commandBots
    :: Monad m
    => [Command m]
    -> Bot m ()
commandBots cs = mergeBots . map commandBot $ helpBot cs : cs

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
