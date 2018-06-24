module Main where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe
import Packs
import State
import Format
import Timestamp
import Preview

main :: IO ()
main = do
   t <- getCurrentTime >>= return . toGregorian . utctDay
   let ts = toTS t
   let state = currentFormat ts
   let out = (printLegal state) ++ [""] ++ (printPreview $ getPreview ts)
   mapM_ putStrLn $ out

toTS :: (Integer, Int, Int) -> Timestamp
toTS (y,m,d) = Ts (fromIntegral d) (fromIntegral m) y
