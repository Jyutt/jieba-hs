import System.IO
import Control.Monad
import Dictionary
import Graph 

main = do
    handle <- openFile "dict.txt.small" ReadMode
    contents <- hGetContents handle
    let dict = dictFromContents contents
    let snt = "我是台湾人"
    let dag = buildDAG snt dict
    let path = optimalPath dag dict
    (putStrLn . show) path