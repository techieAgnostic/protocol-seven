module Nrdb where

import Packs
import Data.List

nrdbSearch :: ([DataPack], [BigBox]) -> String
nrdbSearch (d, b) =
   "https://netrunnerdb.com/find/?q=e%3A" ++
   intercalate "%7C" (
      sort ((map nrdbPackCodes d) ++
      (map nrdbBoxCodes b))
   )

nrdbPackCodes :: DataPack -> String
nrdbPackCodes Lunar1 = "up"
nrdbPackCodes Lunar2 = "tsb"
nrdbPackCodes Lunar3 = "fc"
nrdbPackCodes Lunar4 = "uao"
nrdbPackCodes Lunar5 = "atr"
nrdbPackCodes Lunar6 = "ts"
nrdbPackCodes Sansan1 = "val"
nrdbPackCodes Sansan2 = "bb"
nrdbPackCodes Sansan3 = "cc"
nrdbPackCodes Sansan4 = "uw"
nrdbPackCodes Sansan5 = "oh"
nrdbPackCodes Sansan6 = "uot"
nrdbPackCodes Mumbad1 = "kg"
nrdbPackCodes Mumbad2 = "bf"
nrdbPackCodes Mumbad3 = "dag"
nrdbPackCodes Mumbad4 = "si"
nrdbPackCodes Mumbad5 = "tlm"
nrdbPackCodes Mumbad6 = "ftm"
nrdbPackCodes Flash1 = "23s"
nrdbPackCodes Flash2 = "bm"
nrdbPackCodes Flash3 = "es"
nrdbPackCodes Flash4 = "in"
nrdbPackCodes Flash5 = "ml"
nrdbPackCodes Flash6 = "qu"
nrdbPackCodes Red1 = "dc"
nrdbPackCodes Red2 = "so"
nrdbPackCodes Red3 = "eas"
nrdbPackCodes Red4 = "baw"
nrdbPackCodes Red5 = "fm"
nrdbPackCodes Red6 = "cd"
nrdbPackCodes Kitara1 = "ss"
nrdbPackCodes Kitara2 = "dtwn"
nrdbPackCodes Kitara3 = "cotc"
nrdbPackCodes Kitara4 = "tdatd"
nrdbPackCodes Kitara5 = "win"
nrdbPackCodes Kitara6 = "ka"

nrdbBoxCodes :: BigBox -> String
nrdbBoxCodes Cc = "cac"
nrdbBoxCodes Hp = "hap"
nrdbBoxCodes Oc = "oac"
nrdbBoxCodes Dd = "dad"
nrdbBoxCodes Td = "td"
nrdbBoxCodes Rr = "rar"
