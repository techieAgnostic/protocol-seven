module Main where

import System.Random
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Packs
import State



main :: IO ()
main = do
   t <- getCurrentTime >>= return . toGregorian . utctDay
   let ts = toTS t
   let state = getCurrentRotation ts
   let out = (printLegal state) ++ [""] ++ (printPreview $ getPreview ts state)
   mapM_ putStrLn $ out
