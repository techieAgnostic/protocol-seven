{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module Main where

import Data.Time.Clock
import Data.Time.Calendar
import Data.List
import Data.Maybe
import Packs
import State
import Format
import Timestamp
import Preview
import Yesod
import Config
import Nrdb
import Text.Lucius
import Utils

data ProtocolSeven = ProtocolSeven

io :: MonadIO io => IO a -> io a
io = liftIO

putStrLnIO :: MonadIO io => String -> io ()
putStrLnIO = io . putStrLn

mkYesod "ProtocolSeven" [parseRoutes|
/ HomeR GET
|]

instance Yesod ProtocolSeven

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
   (year, month, day) <- io $ getCurrentTime >>= return . toGregorian . utctDay
   let nextMonth = (month + 1) `mod` 12
   let ts = toTS (year, month, day)
   let ((i,o),b,r) = currentFormat ts
   let bx = (\(Bq x) -> x)
   let (_,ib,_) = initialRotation
   let bbout = map show $ catMaybes $ inBoth (bx ib) (tail $ bx b)
   let dpout = sort $ map show $ map (\(Ir n) -> n) i
   let pr = getPreview ts
   let (pdi,pdo,bbi,bbo) = extractPreview pr
   let nrdbFormat = nrdbSearch (((map (\(Ir n) -> n) i) ++ (map (\(Or n _) -> n) o)),(catMaybes (bx b)))
   let nrdbIn = nrdbSearch (pdi,catMaybes [bbi])
   let nrdbOut = nrdbSearch (pdo,catMaybes [bbo])
   setTitle "Protocol Seven"
   addScriptRemote "https://fonts.googleapis.com/css?family=Inconsolata"
   toWidget $(whamletFile "header.hamlet")
   toWidget $(whamletFile "format.hamlet")
   toWidget $(whamletFile "about.hamlet")
   toWidget $(whamletFile "footer.hamlet")
   toWidget $(luciusFile "style.lucius")

main :: IO ()
main = warp 80 ProtocolSeven

