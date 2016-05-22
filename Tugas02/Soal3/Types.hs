{-# OPTIONS_GHC -Wall #-}
{-
  Huffman Code - Module Types
  Berisikan tipe data - tipe data yang digunakan
  dalam proses Huffman Encoding dan Decoding

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal3.Types 
( Tree (Leaf,Node),
  Bit (L,R),
  HCode, 
  Table
) where

-- Tipe Data bit HuffmanCode
data Bit = L |
           R 
           deriving (Eq, Show)

-- Tipe Data HuffmanCode, yaitu barisan bits
type HCode = [Bit]

-- Tipe Data Table Huffman
-- Berisi tuple yang berisi dua elemen
-- elemen 1: Karakter
-- elemen 2: HuffmanCode
type Table = [(Char, HCode)]

-- Tipe Data Tree
-- Pada Leaf, berisi elemen karakter dan sebuah nilai int
-- Pada nonLeaf, berisi dua tree anak dan sebuah nilai int 
data Tree = Leaf Char Int | 
            Node Int Tree Tree
