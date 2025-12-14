-- | TODO: this should all be absorbed into advent-of-code-api somehow
module Santabot.Days (
  mkDayForYear,
  daysForYear,
) where

import Advent
import Control.Monad (guard)
import Data.List (unfoldr)

mkDayForYear :: Integer -> Integer -> Maybe Day
mkDayForYear y n = do
  d <- mkDay n
  guard (dayInt d <= maxDayForYear y)
  pure d

daysForYear :: Integer -> [Day]
daysForYear y = unfoldr nextDay 1
 where
  nextDay n = do
    d <- mkDayForYear y n
    pure (d, n + 1)

maxDayForYear :: Integer -> Integer
maxDayForYear y
  | y >= 2025 = 12
  | otherwise = 25
