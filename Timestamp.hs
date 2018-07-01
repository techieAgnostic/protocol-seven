module Timestamp where

data Timestamp = Ts Integer Integer Integer deriving Show

isPreviewSeason :: Timestamp -> Bool
isPreviewSeason (Ts x _ _) = x >= 20

inFuture :: Timestamp -> Timestamp -> Bool
inFuture (Ts d1 m1 y1) (Ts d2 m2 y2)
   | y1 /= y2 = y1 > y2
   | m1 /= m2 = m1 > m2
   | d1 /= d2 = d1 > d2
   | otherwise = False

monthsSince :: Timestamp -> Timestamp -> Integer
monthsSince (Ts d1 m1 y1) (Ts d2 m2 y2)
   | t1 `inFuture` t2 = (12 * (y1 - y2)) + (m1 - m2)
   | otherwise = 0
   where
      t1 = Ts d1 m1 y1
      t2 = Ts d2 m2 y2

toTS :: (Integer, Int, Int) -> Timestamp
toTS (y,m,d) = Ts (fromIntegral d) (fromIntegral m) y
