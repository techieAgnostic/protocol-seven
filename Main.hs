module Main where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import Packs
import State
import Format
import Timestamp

main :: IO ()
main = do
   t <- getCurrentTime >>= return . toGregorian . utctDay
   mapM_ putStrLn $ showState (currentFormat $ toTS t)

toTS :: (Integer, Int, Int) -> Timestamp
toTS (y,m,d) = Ts (fromIntegral d) (fromIntegral m) y

showState :: State -> [String]
showState ((i, o), (Bq b), _) =
   [
        "Legal Packs:\n" ++ concat (map (\(Ir n) -> "   " ++ show n ++ "\n") i)
      , "Legal Boxes:\n" ++ concat (map (\x -> "   " ++ show x ++ "\n") (catMaybes $ tail b))
      , "Rotated Packs:\n" ++ concat (map (\(Or n _) -> "   " ++ show n ++ "\n") o)
      , "Rotated Boxes:\n" ++ concat (map (\x -> "   " ++ show x ++ "\n") (catMaybes $ [head b]))
   ]
