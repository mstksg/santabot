{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module Elfbot (
    eventLink
  , challengeCountdown
  , eventCountdown
  , acknowledgeTick
  ) where

import           Advent                    as Advent
import           Advent.API                as Advent
import           Conduit
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Maybe
import           Data.Set                  (Set)
import           Data.Time                 as Time
import           Elfbot.Bot
import           Servant.API
import           Servant.Client.Core
import           Servant.Links
import           Text.Megaparsec
import           Text.Printf
import           Text.Read                 (readMaybe)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Numeric.Interval          as I


eventLink :: MonadIO m => Command m
eventLink = C
    { cName  = "link"
    , cHelp  = "Get the link to a given event (!link 2017 23, !link 16).  Assumes current year if no year or invalid year given."
    , cParse = askLink
    , cResp  = pure . T.pack . uncurry displayLink
    }
  where
    askLink M{..} = runMaybeT $ do
        day      <- maybe empty pure . listToMaybe . mapMaybe mkDay $ w
        (yr,_,_) <- toGregorian . localDay <$> liftIO aocTime
        let year = fromMaybe yr . find (`S.member` validYears) $ w
        pure (year, day)
      where
        w = mapMaybe (readMaybe . T.unpack) . T.words . T.map clear $ mBody
        clear c
          | isDigit c = c
          | otherwise = ' '

data ChallengeEvent = CEHour
                    | CETenMin
                    | CEMinute
                    | CEStart

challengeCountdown :: Applicative m => Alert m
challengeCountdown = A
    { aTrigger = pure . challengeEvent
    , aResp    = fmap (R "##elfbot-test" . T.pack) . pure . uncurry displayCE
    }
  where
    challengeEvent i = do
        guard $ yy `S.member` validYears
        guard $ mm == 12 || (mm == 11 && dd == 30)
        fmap (first (,yy)) . listToMaybe . mapMaybe (uncurry pick) $ evts
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

eventCountdown :: Applicative m => Alert m
eventCountdown = A
    { aTrigger = pure . countdownEvent
    , aResp    = fmap (R "##elfbot-test" . T.pack) . pure . uncurry displayCE
    }
  where
    countdownEvent i = do
        guard $ y `S.member` validYears
        guard $ LocalTime d midnight `I.member` i
        (,y) <$> daysLeft
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
    , aResp    = pure . R "##elfbot-test" . T.pack . show
    }

validYears :: Set Integer
validYears = S.fromList [2015..2019]

displayLink :: Integer -> Advent.Day -> String
displayLink yr day = u
  where
    rp :<|> _ = allLinks adventAPI yr
    rd :<|> _ = rp day
    u = showBaseUrl $ aocBase { baseUrlPath = show (linkURI rd) }
