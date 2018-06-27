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

showMonth :: Int -> String
showMonth 1 = "January"
showMonth 2 = "February"
showMonth 3 = "March"
showMonth 4 = "April"
showMonth 5 = "May"
showMonth 6 = "June"
showMonth 7 = "July"
showMonth 8 = "August"
showMonth 9 = "September"
showMonth 10 = "October"
showMonth 11 = "November"
showMonth 12 = "December"

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
   setTitle "Protocol Seven"
   toWidget [lucius|
      html {
         color: #FFFFFF;
         background-color: #222222;
         font-family: 'Inconsolata', monospace;
         margin: 0px;
         padding: 0px;
      }

      body {
         width: 740px;
         margin: auto;
         padding: 20px;
         text-align: left;
         border-left: 4px solid white;
         border-right: 4px solid white;
      }

      section {
         border-top: 4px solid white;
         padding: 5px;
      }

      header {
         margin: 0px;
         padding: 5px;
         border-top: 4px solid white;
      }

      .subheading {
         text-align: center;
         padding: 5px;
      }

      h1 {
         text-align: center;
         margin: 0px;
         padding: 5px;
      }

      p,h2,h3 {
         margin: 0px;
         padding: 5px; 
      }

      ul {
         text-align: initial;
      }

      .current {
         width: 600px;
      }

      .currentFormat {
         margin: auto;
         padding: 1px;
      }

      .currentLeft {
         width: 300px;
         float: left;
      }

      .currentRight {
         margin-left: 300px;
      }

      .upcoming {
         width: 600px;
      }

      .changes {
         margin: auto;
         padding: 1px;
      }

      .upcomingIn {
         color: #00DD00;
         background-color: inherit;
         width: 300px;
         float: left;
      }

      .upcomingOut {
         color: #DD0000;
         background-color: inherit;
         margin-left: 300px;
      }
   |]
   addScriptRemote "https://fonts.googleapis.com/css?family=Inconsolata"
   toWidgetBody [hamlet|
      <header>
         <h1>Protocol Seven
         <p class="subheading">(Formerly the <em>Chris Hay Honourary Format</em>)</p>
   |]
   toWidgetBody [hamlet|
      <section class="rotation">
         <div class="current">
            <h2>Format for #{showMonth month} #{year}:
            <div class="currentFormat">
               <div class="currentLeft">
                  <h3>Evergreen:
                  <ul>
                     <li>Revised Core Set x3
                  <h3>Big Boxes:
                  <ul>
                     $forall bb <- bbout
                        <li>#{bb}
               <div class="currentRight">
                  <h3>Data-packs:
                  <ul>
                     $forall dp <- dpout
                        <li>#{dp}
         <div class="upcoming">
            <h2>Upcoming Changes for #{showMonth nextMonth} #{year}:
            $maybe (pin, pout, pbin, pbout) <- pr
               <div class="changes">
                  <div class="upcomingIn">
                     <h3>In:
                     <ul>
                        $maybe pbbin <- pbin
                           <li>+ #{show pbbin}
                        $forall indp <- pin
                           <li>+ #{show indp}
                  <div class="upcomingOut">
                     <h3>Out:
                     <ul>
                        $maybe pbbout <- pbout
                           <li>- #{show pbbout}
                        $forall outdp <- pout
                           <li>- #{show outdp}
            $nothing
               <h3>Coming soon!
   |]
   toWidgetBody [hamlet|
      <section class="about">
         <h2>What is Protocol Seven?
         <blockquote><em>"No matter where you go, everybody's connected."</em> - Lain Iwakura
         <p>For many of us, Fantasy Flight Games' Living Card Game <strong>Android: Netrunner</strong> was more than simply a card game. The term <em>Lifestyle Game</em> has been thrown around a lot, but is truer for no other community than that of <strong>ANR</strong>. Without the influx of things to discuss each month, it may seem likely that the community will die, however this is only true if we let it happen.
         <p>With the cancellation of many players favourite game, and no real replacement anywhere on the horizon, all that's left is to create our own points of discussion. Many players are working on custom cycles to expand the card pool, however one <strong>Chris Hay</strong> of the small and oft ridiculed meta from <strong>Dunedin, New Zealand</strong> has come up with a solution; an everchanging format using the current completed card pool.
         <h2>How does it work?
         <blockquote><em>"One theory says that man is a neoteny and is no longer able to evolve. If this is true, then what an absurd creature mankind has evolved into."</em> - Eiri Masami
         <p><strong>Protocol Seven</strong> is an attempt at a constantly evolving metagame. Rather than relying on an influx of new cards every month, <strong>Protocol Seven</strong> relies on a monthly rotation schedule. This allows discussion of a new metagame every month for the forseeable future.
         <p>A month's format in <strong>Protocol Seven</strong> will at any time consist of the following:
         <ul>
            <li>3x Revised Core Set
            <li>18x Data Packs from the Lunar - Kitara cycles
            <li>5-6x Deluxe Boxes (Including Terminal Directive)
         <p>This website will automatically update with the latest format, as well as a preview of the next months changes. This allows discussion, an ever-changing metagame, whilst also keeping the card pool similar enough between months that decks can be altered rather than completely remade.
         <h2>The Protocol Seven Protocol
         <blockquote><em>"But a protocol is nothing more than a simple agreement."</em> - Eiri Masami
         <p>A comprehensive description of the <strong>Protocol Seven</strong> format
         <ul>
            <li>3x Revised Core Sets will be legal at all times
            <li>The most recent FFG Most Wanted List will be active at all times
            <li>The Genesis and Spin cycles are not legal
            <li>Cards from the Original Core Set that are not in the Revised Core Set are not legal
            <li>18x of the 36 Data Packs from Lunar cycle onwards will be legal at all times
            <li>Each rotation will remove the two Data Packs that have been in play the longest, and replace them with two, randomly chosen packs
            <li>Once rotated out, a Data Pack will not rotate in for at least three months
            <li>Each rotation, one Deluxe Box (Including Terminal Directive), in release order, will be chosen as illegal
            <li>Every 7th rotation, all Deluxes will be legal instead
            <li>The rotation will happen on the 1st of every month, New Zealand time
            <li>On the 20th of every month, a preview of the next months format will be shown
            <li><del>No matter what the currently legal packs are, Door to Door (Escalation #59) will always be legal</del>
   |]

main :: IO ()
main = warp 80 Chhf

toTS :: (Integer, Int, Int) -> Timestamp
toTS (y,m,d) = Ts (fromIntegral d) (fromIntegral m) y
