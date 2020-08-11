module Jieba.Dictionary where

import qualified Data.Map.Strict as Map

type Key = String
type EntryPair a = (Key, a)

data Dictionary a m = Dictionary
    { dictMap :: Map.Map Key a
    , metadata :: m
    }

lookup :: Dictionary a m -> Key -> Maybe a
lookup d k = Map.lookup k (dictMap d)

lookupWith :: (a -> b) -> Dictionary a m -> Key -> Maybe b
lookupWith f d k = f <$> Map.lookup k (dictMap d)
