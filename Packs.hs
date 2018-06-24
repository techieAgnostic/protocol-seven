module Packs where

data BigBox =
   cc | hp | oc | dd | td | rr

instance Show BigBox where
   Show cc = "Creation and Control"
   Show hp = "Honor and Profit"
   Show oc = "Order and Chaos"
   Show dd = "Data and Destiny"
   Show td = "Terminal Directive"
   Show rr = "Reign and Reverie"

data DataPack =
     lunar1  | lunar2  | lunar3  | lunar4  | lunar5  | lunar6
   | sansan1 | sansan2 | sansan3 | sansan4 | sansan5 | sansan6
   | mumbad1 | mumbad2 | mumbad3 | mumbad4 | mumbad5 | mumbad6
   | flash1  | flash2  | flash3  | flash4  | flash5  | flash6
   | red1    | red2    | red3    | red4    | red5    | red6
   | kitara1 | kitara2 | kitara3 | kitara4 | kitara5 | kitara6

instance Show DataPack where
   Show lunar1 = "Upstalk"
   Show lunar2 = "The Spaces Between"
   Show lunar3 = "First Contact"
   Show lunar4 = "Up and Over"
   Show lunar5 = "All That Remains"
   Show lunar6 = "The Source"
   Show sansan1 = "The Valley"
   Show sansan2 = "Breaker Bay"
   Show sansan3 = "Chrome City"
   Show sansan4 = "The Underway"
   Show sansan5 = "Old Hollywood"
   Show sansan6 = "The Universe of Tomorrow"
   Show mumbad1 = "Kala Ghoda"
   Show mumbad2 = "Business First"
   Show mumbad3 = "Democracy and Dogma"
   Show mumbad4 = "Salsette Island"
   Show mumbad5 = "The Liberated Mind"
   Show mumbad6 = "Fear the Masses"
   Show flash1 = "23 Seconds"
   Show flash2 = "Blood Money"
   Show flash3 = "Escalation"
   Show flash4 = "Intervention"
   Show flash5 = "Martial Law"
   Show flash6 = "Quorum"
   Show red1 = "Daedalus Complex"
   Show red2 = "Station One"
   Show red3 = "Earth's Scion"
   Show red4 = "Blood and Water"
   Show red5 = "Free Mars"
   Show red6 = "Crimson Dust"
   Show kitara1 = "Sovereign Sight"
   Show kitara2 = "Down the White Nile"
   Show kitara3 = "Council of the Crest"
   Show kitara4 = "The Devil and the Dragon"
   Show kitara5 = "Whispers in Nalubaale"
   Show kitara6 = "Kampala Ascendent"
