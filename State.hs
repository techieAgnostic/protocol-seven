module State where

data Timestamp = Ts Integer Integer Integer

type State = (Pool, BoxQueue, StdGen)

data InRot = Ir DataPack
data OutRot = Or DataPack Integer
data BoxQueue = Bq [Maybe BigBox]

type Pool = ([InRot], [OutRot])

createInRot :: [DataPack] -> [InRot]
createInRot x = map (\n -> Ir n) x

createOutRot :: [DataPack] -> [OutRot]
createOutRot x = map (\n -> Or n 0) x
