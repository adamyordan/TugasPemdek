{-# OPTIONS_GHC -Wall #-}
{-
  Huffman Code - Module Coding
  Berisikan fungsi untuk melakukan encoding dan decoding

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal3.Coding (codeMessage, decodeMessage) where

import Soal3.Types (Tree (Leaf,Node), Bit (L,R), HCode, Table)

-- Menerima argumen Table dan String message,
-- encode pesan berdasarkan tabel HuffmanCode 
codeMessage :: Table -> [Char] -> HCode
codeMessage tbl = concat . map (lookupTable tbl)

-- Mencari kode Huffman dari suatu karakter di Tabel
lookupTable :: Table -> Char -> HCode
lookupTable [] _ = error "lookupTable"
lookupTable ((ch,n):tb) c
  | ch == c   = n
  | otherwise = lookupTable tb c

-- Menerima argumen Tree dan HuffmanCode
-- Meng-decode barisan HuffmanCode tersebut menggunakan tree
-- sehingga menghasilkan suatu string pesan
decodeMessage :: Tree -> HCode -> [Char]
decodeMessage tr = decodeByt tr
  where
  decodeByt (Node _ t1 _) (L:rest) = decodeByt t1 rest
  decodeByt (Node _ _ t2) (R:rest) = decodeByt t2 rest
  decodeByt (Leaf c _) rest = c : decodeByt tr rest
  decodeByt _ [] = []