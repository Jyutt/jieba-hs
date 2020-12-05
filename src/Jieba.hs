module Jieba where

import Jieba.Graph
import Jieba.Types.PosTag
import Jieba.Dictionary.FreqDict
import Jieba.Dictionary.HmmDict
import Jieba.Cut as Cut
import Data.Array
import Data.Maybe (fromMaybe)

cutNoHMM :: FreqDict -> String -> [String]
cutNoHMM = Cut.cutNoHMM

cutHMM :: FreqDict -> HmmDict -> String -> [String]
cutHMM = Cut.cutHMM

cutAll :: FreqDict -> String -> [String]
cutAll = Cut.cutAll

tokenize :: FreqDict -> [String] -> [(String, PosTag)]
tokenize dict xs = zip xs $ map (fromMaybe (Unknown "") . lookupPOS dict) xs
