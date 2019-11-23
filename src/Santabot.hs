{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module Santabot (
    puzzleLink
  , nextPuzzle
  , challengeCountdown
  , eventCountdown
  , acknowledgeTick
  ) where

import           Advent                     as Advent
import           Advent.API                 as Advent
import           Conduit
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Set                   (Set)
import           Data.Time                  as Time
import           Santabot.Bot
import           Servant.API
import           Servant.Client.Core
import           Servant.Links
import           Text.Megaparsec
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.Duration              as DD
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Numeric.Interval           as I


puzzleLink :: MonadIO m => Command m
puzzleLink = C
    { cName  = "link"
    , cHelp  = "Get the link to a given puzzle (!link 23, !link 2017 16).  Gives the most recent released puzzle of that day, unless a valid year is given."
    , cParse = askLink
    , cResp  = pure . T.pack . uncurry displayLink
    }
  where
    askLink M{..} = runExceptT $ do
        day      <- maybe (throwE "No valid day given") pure . listToMaybe . mapMaybe mkDay $ w
        hasDays  <- M.keysSet . M.filter (S.member day) <$> liftIO allPuzzles
        let givenYear = find (`S.member` hasDays) w
            trueYear  = case givenYear of
              Just k | k `S.member` hasDays -> k
              _                             -> S.findMax hasDays
        pure (trueYear, day)
      where
        w = mapMaybe (readMaybe . T.unpack) . T.words . T.map clear $ mBody
        clear c
          | isDigit c = c
          | otherwise = ' '

allPuzzles :: IO (Map Integer (Set Advent.Day))
allPuzzles = do
    vy <- validYears
    sequence . flip M.fromSet vy $ \y -> S.fromList <$>
      filterM (challengeReleased y) (Day <$> finites)

nextPuzzle :: MonadIO m => Command m
nextPuzzle = C
    { cName  = "next"
    , cHelp  = "Display the time until the next puzzle release."
    , cParse = \_ -> pure $ Right ()
    , cResp  = const printNext
    }
  where
    printNext = do
      t <- liftIO aocTime
      let (y, d)    = nextDay (localDay t)
          nextTime  = LocalTime (fromGregorian y 12 (fromIntegral (dayInt d))) midnight
          dur       = realToFrac $ nextTime `diffLocalTime` t
          durString = T.unpack . T.strip . T.pack
                    $ DD.humanReadableDuration dur
      pure . T.pack $ printf
        "Next puzzle (%d Day %d) will be released in %s."
        y
        (dayInt d)
        durString
    nextDay (toGregorian->(y,m,d))
      | m < 12    = (y, minBound)
      | otherwise = case mkDay (fromIntegral d) of
          Nothing -> (y + 1, minBound)
          Just d' -> (y    , d'      )

data ChallengeEvent = CEHour
                    | CETenMin
                    | CEMinute
                    | CEStart

challengeCountdown :: MonadIO m => Alert m
challengeCountdown = A
    { aTrigger = challengeEvent
    , aResp    = pure . T.pack . uncurry displayCE
    }
  where
    challengeEvent i = runMaybeT $ do
        guard . (yy `S.member`) =<< liftIO validYears
        guard $ mm == 12 || (mm == 11 && dd == 30)
        maybe empty pure
          . fmap (first (,yy))
          . listToMaybe
          . mapMaybe (uncurry pick)
          $ evts
      where
        d = localDay $ I.sup i
        (yy,mm,dd) = toGregorian d
        evts =
          [ (LocalTime d midnight           , (,CEStart ) <$> mkDay (fromIntegral dd    ))
          , (LocalTime d (TimeOfDay 11 0  0), (,CEHour  ) <$> mkDay (fromIntegral dd + 1))
          , (LocalTime d (TimeOfDay 11 50 0), (,CETenMin) <$> mkDay (fromIntegral dd + 1))
          , (LocalTime d (TimeOfDay 11 59 0), (,CEMinute) <$> mkDay (fromIntegral dd + 1))
          ]
        pick t e = guard (t `I.member` i) *> e
    displayCE (d, yr) = \case
      CEHour   -> printf "One hour until Day %d challenge!" (dayInt d)
      CETenMin -> printf "Ten minutes until Day %d challenge!" (dayInt d)
      CEMinute -> printf "One minute until Day %d challenge!" (dayInt d)
      CEStart  -> printf "Day %d challenge now online at %s !" (dayInt d) (displayLink yr d)

eventCountdown :: MonadIO m => Alert m
eventCountdown = A
    { aTrigger = countdownEvent
    , aResp    = pure . T.pack . uncurry displayCE
    }
  where
    countdownEvent i = runMaybeT $ do
        guard . (y `S.member`) =<< liftIO validYears
        guard $ LocalTime d midnight `I.member` i
        maybe empty pure $ (,y) <$> daysLeft
      where
        d        = localDay $ I.sup i
        (y,_,_)  = toGregorian d
        daysLeft = packFinite @14 $ (fromGregorian y 12 1 `diffDays` d) - 1

    displayCE d = printf "%d day%s left until Advent of Code %y!" n suff
      where
        n = getFinite d + 1
        suff | n == 1    = "" :: String
             | otherwise = "s"

acknowledgeTick :: Applicative m => Alert m
acknowledgeTick = A
    { aTrigger = pure . Just
    , aResp    = pure . T.pack . show
    }

validYears :: IO (Set Integer)
validYears = do
    (y, _, _) <- toGregorian . localDay <$> aocTime
    pure $ S.fromList [2015 .. y]

displayLink :: Integer -> Advent.Day -> String
displayLink yr day = u
  where
    rp :<|> _ = allLinks adventAPI yr
    rd :<|> _ = rp day
    u = showBaseUrl $ aocBase { baseUrlPath = show (linkURI rd) }
