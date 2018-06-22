module Main2 where

import System.Random
import Packs

data Timestamp = Ts Integer Integer Integer
data DataPack = Dp String Integer deriving (Eq, Show)

initialTimestamp :: Timestamp
initialTimestamp = Ts 29 09 1996

seed :: Int
seed = 69420

initialRNG :: StdGen
initialRNG = mkStdGen $ seed

isPreviewSeason :: Timestamp -> Bool
isPreviewSeason (Ts x _ _) = x >= 20

inFuture :: Timestamp -> Timestamp -> Bool
inFuture (Ts d1 m1 y1) (Ts d2 m2 y2)
   | y1 /= y2 = y1 > y2
   | m1 /= m2 = m1 > m2
   | d1 /= d2 = d1 > d2
   | otherwise = False

monthsSince :: Timestamp -> Timestamp -> Integer
monthsSince (Ts d1 m1 y1) (Ts d2 m2 y2)
   | t1 `inFuture` t2 = (12 * (y1 - y2)) + (m1 - m2)
   | otherwise = 0
   where
      t1 = Ts d1 m1 y1
      t2 = Ts d2 m2 y2

createDpList :: Integer -> [String] -> [DataPack]
createDpList t x = map (\n -> Dp n t) x

createLegalList :: [String] -> [DataPack]
createLegalList x = createDpList (-1) x

createPoolList :: [String] -> [DataPack]
createPoolList x = createDpList 0 x

legalPoolChoices :: [DataPack] -> [DataPack]
legalPoolChoices x = filter (\(Dp _ n) -> n == 0) x

updatePackAge :: [DataPack] -> [DataPack]
updatePackAge p = map (\(Dp s n) -> (Dp s (max 0 (n-1)))) p

setTimer :: Integer -> DataPack -> DataPack
setTimer x (Dp n _) = Dp n x

setLegalTimer :: DataPack -> DataPack
setLegalTimer d = setTimer 3 d

setInRotTimer :: DataPack -> DataPack
setInRotTimer d = setTimer (-1) d

dropOldestLegal :: ([DataPack], [DataPack], StdGen) -> ([DataPack], [DataPack], StdGen)
dropOldestLegal (p, l, r) = (newPool, (drop 2 l), r) 
   where
      newPool = p ++ (map setLegalTimer (take 2 l))

addNewPack :: ([DataPack], [DataPack], StdGen) -> ([DataPack], [DataPack], StdGen)
addNewPack (p, l, r) = (pPool, l ++ [(setInRotTimer nPack)], nR)
   where
      nPool = length $ legalPoolChoices p
      (iPack, nR) = randomR (0, nPool-1) r
      nPack = (legalPoolChoices p) !! iPack
      nPackName = (\(Dp n _) -> n) nPack
      pPool = filter (\(Dp n _) -> n /= nPackName) p

newRotation :: ([DataPack], [DataPack], StdGen) -> ([DataPack], [DataPack], StdGen)
newRotation (p, l, r) = addNewPack . addNewPack $ dropOldestLegal (updatePackAge p, l, r)

changes :: Eq a => [a] -> [a] -> [a]
changes x y = filter (\n -> not $ n `elem` y) x

diffRot :: [DataPack] -> [DataPack] -> ([DataPack], [DataPack])
diffRot n o = (changes n o, changes o n)

initialRotation :: ([DataPack], [DataPack], StdGen)
initialRotation = (
     (createPoolList $ drop n dataPacks)
   , (createLegalList $ take n dataPacks)
   , initialRNG
   )
   where n = (length dataPacks) `div` 2

strictApplyN :: Integer -> (a -> a) -> a -> a
strictApplyN 0 _ x = x
strictApplyN n f x = strictApplyN (n - 1) f $! (f x)

getCurrentRotation :: Timestamp -> ([DataPack], [DataPack], StdGen)
getCurrentRotation t = strictApplyN n newRotation initialRotation
   where
      n = t `monthsSince` initialTimestamp

getPreview :: Timestamp -> ([DataPack], [DataPack], StdGen) -> Maybe ([DataPack], [DataPack])
getPreview n (p, l, r)
   | isPreviewSeason n = Just $ diffRot nl l
   | otherwise = Nothing
   where
      (_, nl, _) = newRotation (p, l, r)
