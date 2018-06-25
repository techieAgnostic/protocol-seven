module State where

import System.Random
import Packs

type State = (Pool, BoxQueue, StdGen)

data InRot = Ir DataPack deriving (Show, Eq)

data OutRot = Or DataPack Integer
instance Eq OutRot where
   (Or d1 _) == (Or d2 _) = d1 == d2

data BoxQueue = Bq [Maybe BigBox]

type Pool = ([InRot], [OutRot])

createInRot :: [DataPack] -> [InRot]
createInRot x = map (\n -> Ir n) x

createOutRot :: [DataPack] -> [OutRot]
createOutRot x = map (\n -> Or n 0) x
