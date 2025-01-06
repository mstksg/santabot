{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Santabot.Run.IRC (
  launchIRC,
  respRaw,
  splitAll,
  splitUntil,
) where

import Advent
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Conduit hiding (connect)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.TQueue
import Data.List (mapAccumL)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Haskell.Printf as P
import Network.SimpleIRC as IRC
import Network.SimpleIRC.Sasl as IRC
import Santabot.Bot
import System.IO

-- | IRC configuration for simpleirc.  Specifies server, name,
-- channels, and the Privmsg handler.
ircConf ::
  String ->
  [String] ->
  String ->
  Maybe String ->
  MVar () ->
  TBMQueue Event ->
  IrcConfig
ircConf serv channels nick pwd started eventQueue =
  (mkDefaultConfig serv nick)
    { cChannels = channels
    , cPass = pwd
    , cEvents = [Privmsg onMessage, Disconnect onDisc, Notice begin]
    }
  where
    onMessage _ IrcMessage{..} = void . runMaybeT $ do
      room <- T.unpack . T.decodeUtf8 <$> maybe empty pure mOrigin
      user <- T.unpack . T.decodeUtf8 <$> maybe empty pure mNick
      let splitMsg =
            map T.unwords
              . splitAll (splitUntil T.length 200)
              $ T.words body
      lift . forM_ splitMsg $ \msg -> do
        atomically $
          writeTBMQueue eventQueue $
            EMsg
              M
                { mRoom = room
                , mUser = user
                , mBody = msg
                }
      where
        body = T.decodeUtf8 mMsg
    onDisc _ = atomically $ closeTBMQueue eventQueue
    begin _ _ = void $ tryPutMVar started ()

splitAll :: ([a] -> ([a], [a])) -> [a] -> [[a]]
splitAll f = go
  where
    go xs = case f xs of
      (ys, zs)
        | null ys || null zs -> [ys]
        | otherwise -> ys : go zs

splitUntil ::
  (a -> Int) ->
  Int ->
  [a] ->
  ([a], [a])
splitUntil f i =
  bimap (map snd) (map snd)
    . span ((<= i) . fst)
    . snd
    . mapAccumL
      ( \sm x ->
          let sm' = sm + f x
           in (sm', (sm', x))
      )
      0

-- | Begin the IRC process with stdout logging.
launchIRC ::
  -- | server
  String ->
  -- | channels to join
  [String] ->
  -- | nick
  String ->
  -- | password
  Maybe String ->
  -- | use sasl plain
  Bool ->
  -- | tick delay (microseconds)
  Int ->
  Bot IO () ->
  IO ()
launchIRC serv channels nick pwd useSasl tick bot = do
  eventQueue <- atomically $ newTBMQueue 1_000_000
  started <- newEmptyMVar
  let cfg =
        (ircConf serv channels nick pwd started eventQueue)
          { cSasl = do
              guard useSasl
              p <- pwd
              pure . saslPlain $ SaslPlainArgs Nothing nick p
          , cUsername = nick
          }

  irc <- either throwIO pure =<< connect cfg True True

  _ <- forkIO $ do
    () <- takeMVar started
    threadDelay 5_000_000
    forever do
      threadDelay tick
      hFlush stdout
      t <- aocServerTime
      atomically $ writeTBMQueue eventQueue (ETick t)

  runConduit $
    sourceTBMQueue eventQueue
      .| bot
      .| C.map respRaw
      .| C.mapM_ (\r -> sendRaw irc r *> threadDelay 500_000)
      .| C.sinkNull

-- | Need this until https://github.com/dom96/SimpleIRC/pull/29 is merged
respRaw :: Resp -> BS.ByteString
respRaw R{..} = T.encodeUtf8 . T.pack $ case rType of
  RTMessage -> [P.s|PRIVMSG %s : %s|] rRoom (T.unpack rBody)
  RTAction -> [P.s|PRIVMSG %s :\SOHACTION %s\SOH|] rRoom (T.unpack rBody)
  RTNotice -> [P.s|NOTICE  %s :%s|] rRoom (T.unpack rBody)
