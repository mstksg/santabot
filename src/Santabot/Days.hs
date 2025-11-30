{-# LANGUAGE OverloadedStrings #-}

module Santabot.Days (
  maxDayForYear,
  mkDayForYear,
  daysForYear,
) where

import Advent
import Control.Monad (guard)
import Data.Finite (finites)

maxDayForYear :: Integer -> Int
maxDayForYear y
  | y >= 2025 = 12
  | otherwise = 25

mkDayForYear :: Integer -> Integer -> Maybe Day
mkDayForYear y n = do
  d <- mkDay n
  guard (dayInt d <= maxDayForYear y)
  pure d

daysForYear :: Integer -> [Day]
daysForYear y = filter ((<= maxDayForYear y) . dayInt) (Day <$> finites)
