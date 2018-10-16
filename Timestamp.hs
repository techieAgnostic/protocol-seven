module Timestamp where

{---
 - Timestamp Module
 -
 - Functions related to getting and using timestamps.
 -
 - Shaun Kerr
 -}

import Data.Time.LocalTime
import Data.Time.Format

-- Internal Date Representation
data Timestamp = Ts Integer Integer Integer deriving Show

-- Previews Start on the 20th
isPreviewSeason :: Timestamp -> Bool
isPreviewSeason (Ts x _ _) = x >= 20

-- True // False if first date is in the future
-- relative to the second date.
inFuture :: Timestamp -> Timestamp -> Bool
inFuture (Ts d1 m1 y1) (Ts d2 m2 y2)
   | y1 /= y2 = y1 > y2
   | m1 /= m2 = m1 > m2
   | d1 /= d2 = d1 > d2
   | otherwise = False

-- How many months in the future the first date is
-- relative from the second date.
-- returns 0 otherwise.
monthsSince :: Timestamp -> Timestamp -> Integer
monthsSince (Ts d1 m1 y1) (Ts d2 m2 y2)
   | t1 `inFuture` t2 = (12 * (y1 - y2)) + (m1 - m2)
   | otherwise = 0
   where
      t1 = Ts d1 m1 y1
      t2 = Ts d2 m2 y2

-- Helper, format Integer representation to Timestamp
toTS :: (Integer, Integer, Integer) -> Timestamp
toTS (y,m,d) = Ts d m y

-- IO Function, get the current date as a [Char]
getCurrentTime :: IO [Char]
getCurrentTime = getZonedTime
   >>= return . (formatTime defaultTimeLocale "%Y %m %d")

-- Helper, format the [Char] as triple Integer
fmtCurrentTime :: [Char] -> (Integer, Integer, Integer)
fmtCurrentTime n = (\[a,b,c] -> (a,b,c)) iTime
   where
      iTime = map (\x -> read x :: Integer) $ words n

-- Hide away some ugly steps. Not sure we ever
-- use the other two so can probably compact this.
getTimestamp :: [Char] -> Timestamp
getTimestamp x = toTS . fmtCurrentTime $ x
