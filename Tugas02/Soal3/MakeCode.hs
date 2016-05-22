{-# OPTIONS_GHC -Wall #-}
{-
  Huffman Code - Module MakeCode
  Modul ini berisikan fungsi yang mengubah sebuah string
  ke sebuah Huffman Tree

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal3.MakeCode (codes, codeTable) where

import Soal3.Types
import Soal3.Frequency ( frequency )
import Soal3.MakeTree ( makeTree )
import Soal3.CodeTable ( codeTable )

-- Mengubah suatu string message ke sebuah Tree Huffman
codes :: [Char] -> Tree
codes = makeTree . frequency