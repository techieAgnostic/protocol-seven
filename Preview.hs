module Preview where

import Data.List
import Data.Maybe
import Packs
import State
import Timestamp
import Format

type Preview = ([DataPack], [DataPack], Maybe BigBox, Maybe BigBox)

extractPreview :: Maybe Preview -> ([DataPack],[DataPack],Maybe BigBox, Maybe BigBox)
extractPreview Nothing = ([],[],Nothing, Nothing)
extractPreview (Just (i,o,ib,ob)) = (i,o,ib,ob)

changes :: Eq a => [a] -> [a] -> [a]
changes x y = filter (\n -> not $ n `elem` y) x

diffRot :: [InRot] -> [InRot] -> ([DataPack], [DataPack])
diffRot c f = (packIn, packOut)
   where
      packIn = map clean $ changes f c
      packOut = map clean $ changes c f
      clean = (\(Ir n) -> n)

getPreview :: Timestamp -> Maybe Preview
getPreview n
   | isPreviewSeason n = Just $ (fst packsChange, snd packsChange, head b, head (rotate 1 b))
   | otherwise = Nothing
   where
      ((i,o), (Bq b), r) = currentFormat n
      packsChange = diffRot i ni
      ((ni,_),_,_) = nextFormat ((i,o),(Bq b),r)

printLegal :: State -> [String]
printLegal ((i,o),(Bq b),_) = [
     "===Evergreen:\nRevised Core Set x3"
   , "===Deluxes  :\n" ++ (intercalate "\n" $ sort $ map show $ catMaybes (tail b))
   , "===Datapacks:\n" ++ (intercalate "\n" $ sort $ map show $ map (\(Ir n) -> n) i)
   ]

printPreview :: Maybe Preview -> [String]
printPreview (Just (i, o, bi, bo)) = [
     "===Upcoming Changes:"
   , ("===In :\n" ++ (intercalate "\n" $ (map show i) ++ (map show cbi)))
   , ("===Out:\n" ++ (intercalate "\n" $ (map show o) ++ (map show cbo)))
   ]
   where
      rmEmp = filter (/="")
      cbi = catMaybes [bi]
      cbo = catMaybes [bo]
      cleanDP = map (\(Or n _) -> n)
printPreview Nothing = []

