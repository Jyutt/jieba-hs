module Jieba.Parsers.Dictionary where

import qualified Jieba.Dictionary as D
import qualified Data.Map.Strict as Map

-- TODO: Parser error handling
dictInputParser :: ([String] -> D.EntryPair a) -> ([D.EntryPair a] -> m)
  -> String -> D.Dictionary a m
dictInputParser words2entry entries2meta contents = D.Dictionary dm meta
  where
    dm = Map.fromList entryPairs
    meta = entries2meta entryPairs
    entryPairs = map (words2entry . words) (lines contents)
