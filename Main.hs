module Main where

import System.Random
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Packs

data Timestamp = Ts Integer Integer Integer
data BigBox = Bb (Maybe String) deriving Show
data DataPack = Dp String Integer deriving Show
type Legal = [DataPack]
type Pool = [DataPack]
type BanQ = [BigBox]
type State = (Legal, Pool, BanQ, StdGen) 

instance Eq DataPack where
   (Dp s1 _) == (Dp s2 _) = s1 == s2

initialTimestamp :: Timestamp
initialTimestamp = Ts 29 09 1996

seed :: Int
seed = 69420

initialRNG :: StdGen
initialRNG = mkStdGen $ seed

initialState :: State
initialState = (
     (createPoolList $ drop n dataPacks)
   , (createLegalList $ take n dataPacks)
   , map (\x -> Bb x) $ (map Just bigBoxes) ++ [Nothing]
   , initialRNG
   )
   where n = (length dataPacks) `div` 2

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

dropOldestLegal :: State -> State
dropOldestLegal (l, p, b, r) = ((drop 2 l), newPool, b, r) 
   where
      newPool = p ++ (map setLegalTimer (take 2 l))

addNewPack :: State -> State
addNewPack (l, p, b, r) = (l ++ [(setInRotTimer nPack)], pPool, b, nR)
   where
      nPool = length $ legalPoolChoices p
      (iPack, nR) = randomR (0, nPool-1) r
      nPack = (legalPoolChoices p) !! iPack
      nPackName = (\(Dp n _) -> n) nPack
      pPool = filter (\(Dp n _) -> n /= nPackName) p

rotate :: Integer -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop (fromIntegral n) (cycle xs)) xs

rotateBigBox :: State -> State
rotateBigBox (l, p, b, r) = (l, p, (rotate 1 b), r)

newRotation :: State -> State
newRotation (l, p, b, r) =
   rotateBigBox . addNewPack . addNewPack $ dropOldestLegal (l, updatePackAge p, b, r)

changes :: Eq a => [a] -> [a] -> [a]
changes x y = filter (\n -> not $ n `elem` y) x

diffRot :: [DataPack] -> [DataPack] -> ([DataPack], [DataPack])
diffRot n o = (changes n o, changes o n)

strictApplyN :: Integer -> (a -> a) -> a -> a
strictApplyN 0 _ x = x
strictApplyN n f x = strictApplyN (n - 1) f $! (f x)

getCurrentRotation :: Timestamp -> State
getCurrentRotation t = strictApplyN n newRotation initialState
   where
      n = t `monthsSince` initialTimestamp

type Preview = ([DataPack], [DataPack], BigBox, BigBox)

getPreview :: Timestamp -> State -> Maybe Preview
getPreview n (l, p, b, r)
   | isPreviewSeason n = Just $ (fst packsChange, snd packsChange, head (rotate 1 b), head b)
   | otherwise = Nothing
   where
      packsChange = diffRot nl l
      (nl, _, _, _) = newRotation (l, p, b, r)

printLegal :: State -> [String]
printLegal (l, p, b, r) = [
     "Evergreen: Revised Core Set x3"
   , "Deluxes  : " ++ (intercalate ", " $ sort $ catMaybes (tail $ map (\(Bb x) -> x) b))
   , "Datapacks: " ++ (intercalate ", " $ sort $ map (\(Dp n _) -> n) l)
   ]

printPreview :: Maybe Preview -> [String]
printPreview (Just (i, o, (Bb bi), (Bb bo))) = [
     ("In : " ++ (intercalate ", " $ (cleanDP i) ++ cbi))
   , ("Out: " ++ (intercalate ", " $ (cleanDP o) ++ cbo))
   ]      
   where
      rmEmp = filter (/="")
      cbi = catMaybes [bi]
      cbo = catMaybes [bo]
      cleanDP = map (\(Dp n _) -> n)
printPreview Nothing = []

toTS :: (Integer, Int, Int) -> Timestamp
toTS (y,m,d) = Ts (fromIntegral d) (fromIntegral m) y

main :: IO ()
main = do
   t <- getCurrentTime >>= return . toGregorian . utctDay
   let ts = toTS t
   let state = getCurrentRotation ts
   let out = (printLegal state) ++ [""] ++ (printPreview $ getPreview ts state)
   mapM_ putStrLn $ out
