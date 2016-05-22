{-# OPTIONS_GHC -Wall #-}
{-
  Huffman Code - Module CodeTable
  Modul ini berisikan fungsi yang melakukan konversi
  sebuah Hufffman Tree ke sebuah tabel yang berisikan
  kode Huffman

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal3.CodeTable (codeTable) where

import Soal3.Types

-- Menerima Argumen HuffmanCode dan Tree untuk dikonversikan ke Table
-- Fungsi ini akan mengecek kode huffman apa yang dibutuhkan untuk
-- mencapai sebuah leaf (karakter)
convert :: HCode -> Tree -> Table
convert cd (Leaf c _) = [(c,cd)]
convert cd (Node _ t1 t2) = (convert (cd++[L]) t1) ++ (convert (cd++[R]) t2)

-- Mengkonversikan Huffman Tree ke Code Tabel
codeTable :: Tree -> Table
codeTable = convert []