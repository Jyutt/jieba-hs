import System.IO
import Jieba.Dictionary.FreqDict
import Jieba
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- hGetContents =<< openFile "dict.txt.small" ReadMode
    let dict = dictFromContents contents
    let snt = "就算失望不能绝望"
    let result = cut NoHMM dict snt
    putStrLn $ intercalate "/" result
