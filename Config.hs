module Config where

{---
 - Config Module
 -
 - Contains settings that will effect the running of the program
 -
 - Shaun Kerr
 -}

import System.Random
import State
import Timestamp
import Packs

-- Earliest rotation.
-- Each format is generated starting from this date.
genesis :: Timestamp
genesis = Ts 29 09 1996

-- Initial seed.
-- Keep it secret. Keep it safe.
seed :: Int
seed = 69420

-- Initial RNG
entropy :: StdGen
entropy = mkStdGen $ seed

-- Initial rotation.
-- Simple split in half, genesis leaves plenty of time to shuffle.
initialRotation :: State
initialRotation = ((i, o), b, r)
   where
      i = createInRot [
           Lunar1, Lunar2, Lunar3, Lunar4, Lunar5, Lunar6
         , Sansan1 , Sansan2 , Sansan3 , Sansan4 , Sansan5 , Sansan6
         , Mumbad1 , Mumbad2 , Mumbad3 , Mumbad4 , Mumbad5 , Mumbad6
         ]
      o = createOutRot [
           Flash1  , Flash2  , Flash3  , Flash4  , Flash5  , Flash6
         , Red1    , Red2    , Red3    , Red4    , Red5    , Red6
         , Kitara1 , Kitara2 , Kitara3 , Kitara4 , Kitara5 , Kitara6
         ]
      b = Bq [Just Cc, Just Hp, Just Oc, Just Dd, Just Td, Just Rr, Nothing]
      r = entropy
