module Utils where

inBoth :: (Eq a) => [a] -> [a] -> [a]
inBoth x y = filter (\n -> n `elem` y) x

showMonth :: Integer -> String
showMonth 1 = "January"
showMonth 2 = "February"
showMonth 3 = "March"
showMonth 4 = "April"
showMonth 5 = "May"
showMonth 6 = "June"
showMonth 7 = "July"
showMonth 8 = "August"
showMonth 9 = "September"
showMonth 10 = "October"
showMonth 11 = "November"
showMonth 12 = "December"

rotate :: Integer -> [a] -> [a]
rotate n xs = take lxs . drop ((fromIntegral n) `mod` lxs) . cycle $ xs
   where
      lxs = length xs

strictApplyN :: Integer -> (a -> a) -> a -> a
strictApplyN 0 _ x = x
strictApplyN n f x = strictApplyN (n - 1) f $! (f x)

changes :: Eq a => [a] -> [a] -> [a]
changes x y = filter (\n -> not $ n `elem` y) x
