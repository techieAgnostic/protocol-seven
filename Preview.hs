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
     "Upcoming Changes:"
   , ("In : " ++ (intercalate ", " $ (cleanDP i) ++ cbi))
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
