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

data Chhf = Chhf

io :: MonadIO io => IO a -> io a
io = liftIO

putStrLnIO :: MonadIO io => String -> io ()
putStrLnIO = io . putStrLn

mkYesod "Chhf" [parseRoutes|
/ HomeR GET
|]

instance Yesod Chhf

inBoth :: (Eq a) => [a] -> [a] -> [a]
inBoth x y = filter (\n -> n `elem` y) x

getHomeR :: Handler Html
getHomeR = do
   t <- io $ getCurrentTime >>= return . toGregorian . utctDay
   let ts = toTS t
   let ((i,o),b,r) = currentFormat ts
   let bx = (\(Bq x) -> x)
   let (_,ib,_) = initialRotation
   let bbout = map show $ catMaybes $ inBoth (bx ib) (tail $ bx b)
   let dpout = sort $ map show $ map (\(Ir n) -> n) i
   let pr = getPreview ts
   defaultLayout [whamlet|
      <h1>CHHF - A Dunedin Netrunner Format
      <h2>About
      <p>The Chris Hay Honourary Format, a post-cancellation Netrunner format. #
         The format consists of 18 packs at any time, and between 5 and 6 big boxes. #
         The following rotation rules are in place:
      <ul>
         <li> Each month, the two packs that have been in rotation longest are rotated out, and two random packs are rotated in
         <li> After a pack rotates out, it cannot rotate in for 3 months
         <li> Each month, in release order, one big box (Including Terminal Directive) is banned for the month
         <li> On the seventh month, all big boxes are legal
      <p> The rotation updates on the first of each month, based on whatever time my server is set to (Probably NZ time but who knows really), and from the 20th of each month onwards, a preview of the upcoming changes will be shown.
      <h2>Current Rotation:
      <h3>Evergreen:
      <ul>
         <li>Revised Core Set x3
      <h3>Big Boxes:
      <ul>
         $forall bb <- bbout
            <li>#{bb}
      <h3>Data-packs:
      <ul>
         $forall dp <- dpout
            <li>#{dp}
      <h2>Upcoming Changes:
      $maybe (pin, pout, pbin, pbout) <- pr
         <h3>In:
         <ul>
            $maybe pbbin <- pbin
               <li>#{show pbbin}
            $forall indp <- pin
               <li>#{show indp}
         <h3>Out:
         <ul>
            $maybe pbbout <- pbout
               <li>#{show pbbout}
            $forall outdp <- pout
               <li>#{show outdp}
      $nothing
         <h3>Coming soon!
   |]

main :: IO ()
main = warp 80 Chhf

toTS :: (Integer, Int, Int) -> Timestamp
toTS (y,m,d) = Ts (fromIntegral d) (fromIntegral m) y
