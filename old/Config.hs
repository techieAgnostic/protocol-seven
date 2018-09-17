module Config where

import System.Random
import State
import Timestamp
import Packs

genesis :: Timestamp
genesis = Ts 29 09 1996

seed :: Int
seed = 69420

entropy :: StdGen
entropy = mkStdGen $ seed

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
