# jieba-hs
![Haskell CI](https://github.com/zyklotomic/jieba-hs/workflows/Haskell%20CI/badge.svg)

TODO:
  - Serious performance issues, see Main.hs for benchmarking program, do some profiling.
  - Most likely due to personal ineptitude with lists and O(n^2) concat

In Beta!

Things that are missing: TF-IDF, Modification of the dictionary at runtime, and async. At least
the things that are glaringly obvious. A lot more is missing.

「結巴-hs」是「[結巴](https://github.com/fxsjy/jieba)」中文分詞的Haskell版本。

"Jieba-hs" is an implementation of the "[Jieba](https://github.com/fxsjy/jieba)"
word segmentation library for Chinese in Haskell.

## 使用 Usage
jieba-hs的字典格式與jieba的一模一樣 ([原版](https://github.com/fxsjy/jieba/tree/master/extra_dict))。
字典在`data/*`。HMM Model看`hmm.model`, 是從[cppjieba](https://github.com/yanyiwu/cppjieba)借的。

The format of the dictionaries are the same as jieba, see the
([original](https://github.com/fxsjy/jieba/tree/master/extra_dict)).
The HMM Model is borrowed from [cppjieba](https://github.com/yanyiwu/cppjieba) with a few
slight modifications.

```haskell
import System.IO
import Dictionary
import Jieba
import Data.List (intercalate)

main :: IO ()
main = do
    contents <- hGetContents =<< openFile "dict.txt.small" ReadMode
    let dict = dictFromContents contents
    let hmmd <- readHmmDict "data/hmm.model"
    let snt = "他来到了网易杭研大厦"
    let result = cutNoHMM dict snt
    let result' = cutHMM dict hmmd snt
    let result'' = cutAll dict snt
    putStrLn $ intercalate "/" result
    putStrLn $ intercalate "/" result'
    putStrLn $ intercalate "/" result''
```
```
*Main> main
他/来到/了/网易/杭/研/大厦 -- No HMM
他/来到/了/网易/杭研/大厦 -- With HMM
他/来/来到/到/了/网/网易/易/杭/研/大/大厦/厦 -- All possible cuts
```

## TODO
- [ ] TF-IDF
- [ ] 使用Trie?
- [ ] QuickCheck unit tests
