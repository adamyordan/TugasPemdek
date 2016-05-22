{-# OPTIONS_GHC -Wall #-}
{-
  Huffman Code - Module MakeTree
  Berisikan fungsi yang menerima input kumpulan karakter
  dan frekuensinya, lalu mengembalikan sebuah Huffman Tree
  berdasarkan frekuensi karakter tersebut.

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal3.MakeTree (makeTree) where

import Soal3.Types

-- Membuat sebuah tree huffman berdasarkan list yang berisi
-- karakter dan frekuensinya
makeTree :: [(Char,Int)] -> Tree
makeTree = makeCodes . toTreeList

-- Mengubah tuple berisi karakter dan frekuensinya ke dalam
-- bentuk tree (leaf)
toTreeList :: [(Char,Int)] -> [Tree]
toTreeList = map (uncurry Leaf)

-- Menyatukan sekumpulan leaf menjadi satu kesatuan tree
makeCodes :: [Tree] -> Tree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

-- Operasi amalgate, yaitu menggabungkan dua tree ter-depan
amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts
amalgamate _ = error "Too small list of Tree"

-- Menggabungkan dua buah tree menjadi satu tree
pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (value t1 + value t2) t1 t2

-- Mengambil nilai value dari suatu tree
value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _ ) = n

-- Memasukkan sebuah tree ke bagian depan list of tree
insTree :: Tree -> [Tree] -> [Tree]
insTree t ts = t:ts