module Config where

genesis :: Timestamp
genesis = Ts 29 09 1996

seed :: Int
seed = 69420

entropy :: StdGen
entropy = mkStdGen $ seed

initialRotation :: State
initialRotation = ((i, o), b, r)
   where
      i = createInRot [
           lunar1, lunar2, lunar3, lunar4, lunar5, lunar6
         , sansan1 , sansan2 , sansan3 , sansan4 , sansan5 , sansan6
         , mumbad1 , mumbad2 , mumbad3 , mumbad4 , mumbad5 , mumbad6
         ]
      o = createOutRot [
           flash1  , flash2  , flash3  , flash4  , flash5  , flash6
         , red1    , red2    , red3    , red4    , red5    , red6
         , kitara1 , kitara2 , kitara3 , kitara4 , kitara5 , kitara6
         ]
      b = Bq [Just rr, Just td, Just dd, Just oc, Just hp, Just cc, Nothing]
      r = entropy
