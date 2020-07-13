import System.IO
import Dictionary
import Graph

main :: IO ()
main = do
    handle <- openFile "dict.txt.small" ReadMode
    contents <- hGetContents handle
    let dict = dictFromContents contents
    putStrLn "Enter sentence: "
    snt <- getLine
    -- let snt = "退回你的我的回不去的悠悠的岁月"
    let dag = buildDAG snt dict
    let path = optimalPath dag dict
    let segmentation = (segmentSentence snt . calculateSegments) path
    mapM_ putStrLn segmentation
