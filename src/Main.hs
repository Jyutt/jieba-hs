import System.IO
--import Jieba.Parsers.FreqDict (dictFromContents)
import Jieba
import Jieba.Parsec.FreqDict
import Data.List (intercalate)

main :: IO ()
main = do
    --contents <- hGetContents =<< openFile "data/dict.txt.small" ReadMode
    --let dict = dictFromContents contents
    dict <- readFreqDict "data/dict.txt.small"
    let snt = "我和我最后的倔强"
    let result = cut NoHMM dict snt
    putStrLn $ intercalate "/" result
