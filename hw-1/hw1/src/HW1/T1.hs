{-# LANGUAGE LambdaCase #-}

module HW1.T1
  ( Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

nextDay :: Day -> Day
nextDay = \case
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays 0 current = current
afterDays cnt current = afterDays (cnt - 1) (nextDay current)

isWeekend :: Day -> Bool
isWeekend = \case
  Saturday -> True
  Sunday   -> True
  _        -> False

daysToParty :: Day -> Natural
daysToParty = go 0
  where
    go acc Friday = acc
    go acc x      = go (acc + 1) (nextDay x)