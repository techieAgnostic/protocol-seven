module Format where

import System.Random
import State
import Timestamp
import Config
import Utils

currentFormat :: Timestamp -> State
currentFormat t = strictApplyN n nextFormat initialRotation
   where
      n = t `monthsSince` genesis

nextFormat :: State -> State
nextFormat (p, b, r) = (np, nb, nr)
   where
      ip = rotateOld p
      (np, nr) = (addNewPack . addNewPack) (ip, r)
      nb = rotateBox b

legalOutRot :: [OutRot] -> [OutRot]
legalOutRot x = filter (\(Or _ n) -> n == 0) x

updatePackAge :: [OutRot] -> [OutRot]
updatePackAge p = map (\(Or s n) -> (Or s $ max 0 (n-1))) p

setIllegal :: InRot -> OutRot
setIllegal (Ir n) = Or n 3

setLegal :: OutRot -> InRot
setLegal (Or n _) = Ir n

rotateOld :: Pool -> Pool
rotateOld (i, o) = (ni, no)
   where
      ni = drop 2 i
      no = (updatePackAge o) ++ dropped
      dropped = map setIllegal (take 2 i)

addNewPack :: (Pool, StdGen) -> (Pool, StdGen)
addNewPack ((i, o), r) = ((ni, no), nr)
   where
      lp = length $ legalOutRot o
      (ip, nr) = randomR (0, lp-1) r
      np = (legalOutRot o) !! ip
      ni = i ++ [(setLegal np)]
      no = filter (\x -> x /= np) o

rotateBox :: BoxQueue -> BoxQueue
rotateBox (Bq x) = Bq $ rotate 1 x
