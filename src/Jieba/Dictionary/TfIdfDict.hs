module Jieba.Dictionary.TfIdfDict where

import Jieba.Dictionary as D
import qualified Data.Map.Strict as Map

type TfIdf = Double
type TfIdfDict = Dictionary TfIdf Empty
type DictEntryPair = (String, TfIdf)

data Empty = Empty

entryPairFromContents :: String -> [DictEntryPair]
entryPairFromContents input = map f $ lines input
  where
    f = words2entry . words
    words2entry xs = (term, tfidf)
      where
        term = head xs
        tfidf = read (xs !! 1) :: TfIdf

dictFromEntryPairs :: [DictEntryPair] -> TfIdfDict
dictFromEntryPairs entryPairs = Dictionary (Map.fromList entryPairs) Empty

lookupTfIdf :: TfIdfDict -> String -> Maybe TfIdf
lookupTfIdf = D.lookup
