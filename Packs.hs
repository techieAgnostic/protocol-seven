module Packs where

{---
 - Pack Module
 -
 - Defines datatypes for each pack and I totally didn't
 - hand code this nuh-uh for real
 -
 - Shaun Kerr
 -}

data BigBox =
   Cc | Hp | Oc | Dd | Td | Rr deriving Eq

instance Show BigBox where
   show Cc = "Creation and Control"
   show Hp = "Honor and Profit"
   show Oc = "Order and Chaos"
   show Dd = "Data and Destiny"
   show Td = "Terminal Directive"
   show Rr = "Reign and Reverie"

data DataPack =
     Lunar1  | Lunar2  | Lunar3  | Lunar4  | Lunar5  | Lunar6
   | Sansan1 | Sansan2 | Sansan3 | Sansan4 | Sansan5 | Sansan6
   | Mumbad1 | Mumbad2 | Mumbad3 | Mumbad4 | Mumbad5 | Mumbad6
   | Flash1  | Flash2  | Flash3  | Flash4  | Flash5  | Flash6
   | Red1    | Red2    | Red3    | Red4    | Red5    | Red6
   | Kitara1 | Kitara2 | Kitara3 | Kitara4 | Kitara5 | Kitara6 deriving Eq

instance Show DataPack where
   show Lunar1 = "Upstalk"
   show Lunar2 = "The Spaces Between"
   show Lunar3 = "First Contact"
   show Lunar4 = "Up and Over"
   show Lunar5 = "All That Remains"
   show Lunar6 = "The Source"
   show Sansan1 = "The Valley"
   show Sansan2 = "Breaker Bay"
   show Sansan3 = "Chrome City"
   show Sansan4 = "The Underway"
   show Sansan5 = "Old Hollywood"
   show Sansan6 = "The Universe of Tomorrow"
   show Mumbad1 = "Kala Ghoda"
   show Mumbad2 = "Business First"
   show Mumbad3 = "Democracy and Dogma"
   show Mumbad4 = "Salsette Island"
   show Mumbad5 = "The Liberated Mind"
   show Mumbad6 = "Fear the Masses"
   show Flash1 = "23 Seconds"
   show Flash2 = "Blood Money"
   show Flash3 = "Escalation"
   show Flash4 = "Intervention"
   show Flash5 = "Martial Law"
   show Flash6 = "Quorum"
   show Red1 = "Daedalus Complex"
   show Red2 = "Station One"
   show Red3 = "Earth's Scion"
   show Red4 = "Blood and Water"
   show Red5 = "Free Mars"
   show Red6 = "Crimson Dust"
   show Kitara1 = "Sovereign Sight"
   show Kitara2 = "Down the White Nile"
   show Kitara3 = "Council of the Crest"
   show Kitara4 = "The Devil and the Dragon"
   show Kitara5 = "Whispers in Nalubaale"
   show Kitara6 = "Kampala Ascendent"
