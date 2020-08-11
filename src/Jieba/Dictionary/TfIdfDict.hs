module Jieba.Dictionary.TfIdfDict where

import Jieba.Dictionary as D

type TfIdf = Double
type TfIdfDict = Dictionary TfIdf Empty
type DictEntryPair = EntryPair TfIdf

data Empty = Empty

lookupTfIdf :: TfIdfDict -> String -> Maybe TfIdf
lookupTfIdf = D.lookup
