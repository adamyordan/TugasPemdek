module Soal3.MakeTree (makeTree) where

import Soal3.Types

makeTree :: [(Char,Int)] -> Tree
makeTree = makeCodes . toTreeList

toTreeList :: [(Char,Int)] -> [Tree]
toTreeList = map (uncurry Leaf)

makeCodes :: [Tree] -> Tree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (value t1 + value t2) t1 t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _ ) = n