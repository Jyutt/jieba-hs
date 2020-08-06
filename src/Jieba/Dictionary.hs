module Jieba.Dictionary where

import qualified Data.Map.Strict as Map

type Key = String
type EntryPair a = (Key, a)

data Dictionary a b = Dictionary
    { dictMap :: Map.Map Key a
    , metadata :: b
    }

lookup :: Dictionary a m -> Key -> Maybe a
lookup d k = Map.lookup k (dictMap d)

lookupWith :: (a -> b) -> Dictionary a m -> Key -> Maybe b
lookupWith f d k = f <$> Map.lookup k (dictMap d)

-- TODO: Parser error handling
dictInputParser :: ([String] -> EntryPair a) -> ([EntryPair a] -> b)
  -> String -> Dictionary a b
dictInputParser words2entry entries2meta contents = Dictionary dm m
  where
    dm = Map.fromList entryPairs
    m = entries2meta entryPairs
    entryPairs = map (words2entry . words) (lines contents)
