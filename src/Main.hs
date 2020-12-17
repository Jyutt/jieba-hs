import System.IO
--import Jieba.Parsers.FreqDict (dictFromContents)
import Jieba
import Jieba.Parsec.FreqDict
import Jieba.Parsec.HmmWeights
import Jieba.FinalSeg
import Jieba.Types.HmmState
import Data.List (intercalate)

main :: IO ()
main = do
    dict <- readFreqDict "data/dict.txt.small"
    hmmd <- readHmmDict "data/hmm.model"
    let snt = "他来到了网易杭研大厦"
    let result = cutNoHMM dict snt
    let result' = cutHMM dict hmmd snt
    putStrLn $ intercalate "/" result
    putStrLn . intercalate "/" $ result'
