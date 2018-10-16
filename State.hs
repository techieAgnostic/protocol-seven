module State where

{---
 - State Module
 - 
 - Functions used for creating and defining the
 - state of the current rotation.
 -
 - Shaun Kerr.
 -}

import System.Random
import Packs

-- Total State is the Datapack Pool, the Banned Big Box,
-- and the next random number.
type State = (Pool, BoxQueue, StdGen)

-- In Rotation packs have no metadata
data InRot = Ir DataPack deriving (Show, Eq)

-- Out Rotation packs need a number of months
-- until they're legal again.
data OutRot = Or DataPack Integer
instance Eq OutRot where
   (Or d1 _) == (Or d2 _) = d1 == d2

-- Box Queue is full of Maybes for the month
-- where none are banned.
data BoxQueue = Bq [Maybe BigBox]

-- Total pool is split into in and out of rotation
type Pool = ([InRot], [OutRot])

-- Simple Wrapper
createInRot :: [DataPack] -> [InRot]
createInRot x = map (\n -> Ir n) x

-- Wrapper + Initial legality
createOutRot :: [DataPack] -> [OutRot]
createOutRot x = map (\n -> Or n 0) x
