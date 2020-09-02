import System.IO
import Jieba.Parsers.FreqDict (dictFromContents)
import Jieba
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- hGetContents =<< openFile "data/dict.txt.small" ReadMode
    let dict = dictFromContents contents
    let snt = "我和我最后的倔强"
    let result = cut NoHMM dict snt
    putStrLn $ intercalate "/" result
