module HW1.T1
  ( Day (..),
    afterDays,
    daysToParty,
    isWeekend,
    nextDay,
  )
where

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
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Natural -> Day -> Day
afterDays num day
  | num == 0 = day
  | otherwise = afterDays (num -1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day = 1 + daysToParty (nextDay day)
