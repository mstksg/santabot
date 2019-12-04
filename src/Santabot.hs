{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module Santabot (
    puzzleLink
  , puzzleThread
  , nextPuzzle
  , challengeCountdown
  , eventCountdown
  , boardCapped
  , privateCapped
  , acknowledgeTick
  , Phrasebook
  , addSantaPhrase
  , validYears
  , dayTitle
  ) where

import           Advent
import           Advent.API                 as Advent
import           Advent.Reddit
import           Advent.Types
import           Conduit
import           Control.Monad
import           Control.Monad.Combinators
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           Data.Time                  as Time
import           Debug.Trace
import           Santabot.Bot
import           Servant.API
import           Servant.Client.Core
import           Servant.Links
import           System.Random
import           Text.Megaparsec
import           Text.Read                  (readMaybe)
import qualified Data.Duration              as DD
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Language.Haskell.Printf    as P
import qualified Numeric.Interval           as I
import qualified Text.Megaparsec.Char.Lexer as P

type Phrasebook = Set Text

puzzleLink :: MonadIO m => Command m
puzzleLink = C
    { cName  = "link"
    , cHelp  = "Get the link to a given puzzle with the puzzle title (!link 23, !link 2017 16).  If bad day or year given, just returns most recent match."
    , cParse = askLink
    , cResp  = \(y,d) -> liftIO $ fancyLink y d (displayLink y d)
    }

puzzleThread :: MonadIO m => Command m
puzzleThread = C
    { cName  = "thread"
    , cHelp  = "Get the link to a given puzzle's reddit discussion thread (!thread 23, !thread 2017 16).  If bad day or year given, just returns most recent match."
    , cParse = askLink
    , cResp  = \(y,d) -> liftIO $ do
        getPostLink y d >>= \case
          Nothing -> pure "Thread not available, sorry!"
          Just u  -> fancyLink y d u
    }

fancyLink :: Integer -> Advent.Day -> String -> IO Text
fancyLink y d url = do
    title <- foldMap ([P.s|: "%s"|] . T.unpack) <$> dayTitle y d
    pure . T.pack $
      [P.s|[%d Day %d%s] %s|] y (dayInt d) title url

askLink
    :: MonadIO m
    => Message
    -> m (Either Text (Integer, Advent.Day))
askLink M{..} = do
    allP <- liftIO allPuzzles
    case listToMaybe (mapMaybe mkDay w) of
      Nothing  -> do
        (y, m, d) <- toGregorian . localDay <$> liftIO aocServerTime
        case mkDay (fromIntegral d) of
          Just dd
            | m == 12
            , dd `S.member` fold (M.lookup y allP)
            -> pure $ Right (y, dd)
          _ -> pure $ Left "No valid day found."
      Just day -> pure . Right $
        let hasDays  = M.keysSet . M.filter (S.member day) $ allP
            givenYear = find (`S.member` hasDays) w
            trueYear  = case givenYear of
              Just k | k `S.member` hasDays -> k
              _                             -> S.findMax hasDays
        in  (trueYear, day)
  where
    w = mapMaybe (readMaybe . T.unpack) . T.words . T.map clear $ mBody
    clear c
      | isDigit c = c
      | otherwise = ' '

dayTitle :: Integer -> Advent.Day -> IO (Maybe Text)
dayTitle y d = runMaybeT $ do
    ps <- MaybeT $ either (const Nothing) Just <$>
      runAoC (defaultAoCOpts y "") (AoCPrompt d)
    p1 <- maybe empty pure $ M.lookup Part1 ps
    either (const empty) pure $
      parse parseTitle "" p1
  where
    parseTitle :: Parsec Void Text Text
    parseTitle = between (try "<h2>") (try "</h2>") $ do
      _ <- "--- Day "
      _ <- P.decimal @_ @_ @_ @Integer
      _ <- ": "
      T.strip . T.pack <$> manyTill anySingle (try " ---")

allPuzzles :: IO (Map Integer (Set Advent.Day))
allPuzzles = do
    vy <- validYears
    sequence . flip M.fromSet vy $ \y -> S.fromList <$>
      filterM (challengeReleased y) (Day <$> finites)

nextPuzzle :: (MonadIO m, MonadReader Phrasebook m) => Command m
nextPuzzle = simpleCommand "next" "Display the time until the next puzzle release." $ do
    t <- liftIO aocServerTime
    let (y, d)    = nextDay (localDay t)
        nextTime  = LocalTime (fromGregorian y 12 (fromIntegral (dayInt d))) midnight
        dur       = traceShowId $ realToFrac $ nextTime `diffLocalTime` t
        durString = T.unpack . T.strip . T.pack
                  $ DD.humanReadableDuration dur
    addSantaPhrase . T.pack $
      [P.s|Next puzzle (%d Day %d) will be released in %s.|]
        y
        (dayInt d)
        durString
  where
    nextDay (toGregorian->(y,m,d))
      | m < 12    = (y, minBound)
      | otherwise = case mkDay (fromIntegral d + 1) of
          Nothing -> (y + 1, minBound)
          Just d' -> (y    , d'      )

data ChallengeEvent = CEHour
                    | CETenMin
                    | CEMinute
                    | CEStart
  deriving Show

challengeCountdown :: (MonadIO m, MonadReader Phrasebook m) => Alert m
challengeCountdown = A
    { aTrigger = pure . challengeEvent
    , aResp    = traverse (addSantaPhrase . T.pack)
               . uncurry displayCE
    }
  where
    challengeEvent i = do
        guard $ (mm == 12) || (mm == 11 && dd == 30)
        first (,yy) <$> do
          listToMaybe . mapMaybe (uncurry pick) $ evts
      where
        d = localDay $ I.sup i
        (yy ,mm, dd ) = toGregorian d
        (_  ,_  ,dd') = toGregorian (succ d)
        evts =
          [ (LocalTime d midnight           , (,CEStart ) <$> mkDay (fromIntegral dd ))
          , (LocalTime d (TimeOfDay 23 0  0), (,CEHour  ) <$> mkDay (fromIntegral dd'))
          , (LocalTime d (TimeOfDay 23 50 0), (,CETenMin) <$> mkDay (fromIntegral dd'))
          , (LocalTime d (TimeOfDay 23 59 0), (,CEMinute) <$> mkDay (fromIntegral dd'))
          ]
        pick t e = guard (t `I.member` i) *> e
    displayCE (d, yr) = \case
      CEHour   -> (False, [P.s|One hour until Day %d challenge!|]    (dayInt d)                   )
      CETenMin -> (False, [P.s|Ten minutes until Day %d challenge!|] (dayInt d)                   )
      CEMinute -> (False, [P.s|One minute until Day %d challenge!|]  (dayInt d)                   )
      CEStart  -> (True , [P.s|Day %d challenge now online at %s !|] (dayInt d) (displayLink yr d))

eventCountdown :: (MonadIO m, MonadReader Phrasebook m) => Alert m
eventCountdown = A
    { aTrigger = pure . countdownEvent
    , aResp    = fmap (True,) . addSantaPhrase . T.pack . uncurry displayCE
    }
  where
    countdownEvent i = do
        guard $ LocalTime d midnight `I.member` i
        guard $ m < 12
        (,y) <$> daysLeft
      where
        d        = localDay $ I.sup i
        (y,m,_)  = toGregorian d
        daysLeft = packFinite @14 $ (fromGregorian y 12 1 `diffDays` d) - 1

    displayCE d = [P.s|%d day%s left until Advent of Code %d!|] n suff
      where
        n = getFinite d + 1
        suff | n == 1    = "" :: String
             | otherwise = "s"

getCapTime :: MonadIO m => Integer -> Advent.Day -> m (Maybe UTCTime)
getCapTime y d = liftIO $ do
    t <- aocServerTime
    putStrLn $ [P.s|[CAP DETECTION] Getting leaderboard cap time for %04d %d at %s|]
                  y (dayInt d) (show t)
    mlb <- either (const Nothing) Just <$>
      runAoC (defaultAoCOpts y "") (AoCDailyLeaderboard d)
    pure $ mlb >>= \lb -> do
      guard $ fullDailyBoard lb
      entries <- NE.nonEmpty . toList . dlbStar2 $ lb
      pure $ maximum (fmap dlbmTime entries)

boardCapped :: MonadIO m => Alert m
boardCapped = risingEdgeAlert "capped" 1 True getCapTime response
  where
    response y d capTime = liftIO $ do
      linkUrl <- getPostLink y d
      let timeString = formatTime defaultTimeLocale "%H:%M:%S EST"
                     . utcToLocalTime (read "EST")
                     $ capTime
          linkUrl'   = fromMaybe "thread not yet available" linkUrl
      pure . T.pack $
        [P.s|Leaderboard for Day %d is now capped at %s (%s)!|]
          (dayInt d) timeString linkUrl'

privateCapped
    :: MonadIO m
    => String     -- ^ session key
    -> Integer    -- ^ leaderboard ID
    -> String     -- ^ leaderboard join code
    -> Alert m
privateCapped tok lbid joinCode = risingEdgeAlert "private-capped" 5 False trigger response
  where
    trigger y d = liftIO $ do
      t <- aocServerTime
      putStrLn $ [P.s|[PRIVATE CAP DETECTION] Getting private leaderboard cap time for %04d %d at %s|]
                    y (dayInt d) (show t)
      mlb <- either (const Nothing) Just <$>
        runAoC (defaultAoCOpts y tok) (AoCLeaderboard lbid)
      pure $ mlb >>= \lb -> do
        let dayMap = M.take 10 . flip foldMap (lbMembers lb) $ \LBM{..} ->
              flip foldMap (M.lookup d lbmCompletion) $ \pts ->
                flip foldMap (M.lookup Part2 pts) $ \tt ->
                  M.singleton tt (lbmId, lbmName)
        guard $ M.size dayMap >= 10
        pure dayMap
    response y d capMap = do
        globalCap <- getCapTime y d
        pure . T.pack $
          [P.s|Congrats to IRC Board Top 10 (%d-%s) of %04d day %d! %s|]
            lbid joinCode y (dayInt d) (T.unpack (lbString globalCap))
      where
        lbString cap = T.intercalate ", " . zipWith mkStr [(1::Int)..] . M.toList $ capMap
          where
            mkStr place (tt, (i, u)) = T.pack $ [P.s|%s%d%s %s (%s)|] openb place closeb uString tString
              where
                uString = maybe ([P.s|Anonymous User #%d|] i) T.unpack u
                tString = formatTime defaultTimeLocale "%H:%M:%S"
                        . utcToLocalTime (read "EST")
                        $ tt
                (openb, closeb) = case cap of
                  Just cc | cc < tt -> ("[", "]")
                  _                 -> ("{", "}")

acknowledgeTick :: Applicative m => Alert m
acknowledgeTick = A
    { aTrigger = pure . Just
    , aResp    = pure . (False,) . T.pack . show
    }

validYears :: IO (Set Integer)
validYears = do
    (y, mm, _) <- toGregorian . localDay <$> aocServerTime
    let y' | mm >= 11  = y
           | otherwise = y - 1
    pure $ S.fromList [2015 .. y']

displayLink :: Integer -> Advent.Day -> String
displayLink yr day = u
  where
    rp :<|> _ = allLinks adventAPI yr
    rd :<|> _ = rp day
    u = showBaseUrl $ aocBase { baseUrlPath = show (linkURI rd) }

addSantaPhrase :: (MonadIO m, MonadReader Phrasebook m) => Text -> m Text
addSantaPhrase txt = do
    phrasebook <- ask
    liftIO $ do
      pick <- (`S.elemAt` phrasebook)
          <$> randomRIO (0, S.size phrasebook - 1)
      pure $ pick <> " " <> txt

