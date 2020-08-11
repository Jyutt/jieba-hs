module Jieba.Parsers.TfIdfDict where

import Jieba.Parsers.Dictionary (dictInputParser)
import qualified Jieba.Dictionary.TfIdfDict as TID

words2entry :: [String] -> TID.DictEntryPair
words2entry xs = (head xs, read (xs !! 1) :: TID.TfIdf)

dictFromContents :: String -> TID.TfIdfDict
dictFromContents = dictInputParser words2entry (const TID.Empty)
