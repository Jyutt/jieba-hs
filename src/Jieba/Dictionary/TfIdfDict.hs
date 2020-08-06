module Jieba.Dictionary.TfIdfDict where

import Jieba.Dictionary as D

type TfIdf = Double
type TfIdfDict = Dictionary TfIdf Empty
type DictEntryPair = EntryPair TfIdf

data Empty = Empty

words2entry :: [String] -> DictEntryPair
words2entry xs = (head xs, read (xs !! 1) :: TfIdf)

dictFromContents :: String -> TfIdfDict
dictFromContents = dictInputParser words2entry (const Empty)

lookupTfIdf :: TfIdfDict -> String -> Maybe TfIdf
lookupTfIdf = D.lookup
