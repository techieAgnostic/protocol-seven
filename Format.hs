module Format where

nextFormat :: State -> State
nextFormat (p, b, r) = (np, nb, nr)
   where
      ip = rotateOld p
      (np, nr) = addNewPack . addNewPack (ip, r)
      nb = rotateBox b

legalOutRot :: [OutRot] -> [OutRot]
legalOutRot x = filter (\(Or _ n) -> n == 0) x

updatePackAge :: [OutRot] -> [OutRot]
updatePackAge p = map (\(Or s n) -> (Or s $ max 0 (n-1)) p

setIllegal :: InRot -> OutRot
setIllegal (Ir n) = Or n 3

setLegal :: OutRot -> InRot
setLegal (Or n _) = Ir n

rotateOld :: Pool -> Pool
rotateOld (i, o) = (ni, no)
   where
      ni = drop 2 i
      no = o ++ dropped
      dropped = map setIllegal (take 2 i)

addNewPack :: (Pool, StdGen) -> (Pool, StdGen)
addNewPack ((i, o), r) = ((ni, no), nr)
   where
      lp = length $ legalOutRot o
      (ip, nr) = randomR (0, lp-1) r
      np = (legalOutRot o) !! ip
      ni = i ++ (setLegal np)
      no = filter (\x -> x /= np) o

rotate :: Integer -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop (fromIntegral n) (cycle xs)) xs

rotateBox :: BoxQueue -> BoxQueue
rotateBox (Bq x) = Bq $ rotate 1 x

changes :: Eq a => [a] -> [a] -> [a]
changes x y = filter (\n -> not $ n `elem` y) x

strictApplyN :: Integer -> (a -> a) -> a -> a
strictApplyN 0 _ x = x
strictApplyN n f x = strictApplyN (n - 1) f $! (f x)
