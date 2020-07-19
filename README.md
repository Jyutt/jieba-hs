# jieba-hs

「結巴-hs」是「[結巴](https://github.com/fxsjy/jieba)」中文分詞的Haskell版本。

## 使用
jieba-hs的字典格式與jieba的一模一樣。可[參考](https://github.com/fxsjy/jieba/tree/master/extra_dict)。

```
GHCi, version 8.6.4: http://www.haskell.org/ghc/  :? for help
Prelude> :l Graph
[1 of 2] Compiling Dictionary       ( Dictionary.hs, interpreted )
[2 of 2] Compiling Graph            ( Graph.hs, interpreted )
Ok, two modules loaded.
*Graph> import System.IO
*Graph System.IO> handle <- openFile "dict.txt.small" ReadMode
*Graph System.IO> contents <- hGetContents handle
*Graph System.IO> let dict = dictFromContents contents
*Graph System.IO> let snt = "就算失望不能绝望"
*Graph System.IO> let dag = buildDAG snt dict
*Graph System.IO> let path = optimalPath dag dict
*Graph System.IO> let segmentation = (segmentSentence snt . calculateSegments) path
*Graph System.IO> mapM_ putStrLn segmentation
就算                                         
失望
不能
绝望

```

## TODO
- [ ] Hidden Markov Model 當有非連通的分量時，使用HMM填空。目前把非通的路徑設成零機率。
- [ ] TF-IDF 的字典
- [ ] POS Tagging
- [ ] jieba-monad 爲了支持不同形式的字典。可能把repo名改成jiebaMonad？
