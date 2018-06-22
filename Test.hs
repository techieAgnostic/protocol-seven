cleanMaybe :: Maybe String -> [String]
cleanMaybe (Just x) = [x]
cleanMaybe Nothing = []

