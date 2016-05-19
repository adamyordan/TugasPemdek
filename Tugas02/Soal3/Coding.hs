module Soal3.Coding (codeMessage, decodeMessage) where

import Soal3.Types (Tree (Leaf,Node), Bit (L,R), HCode, Table)

codeMessage :: Table -> [Char] -> HCode
codeMessage tbl = concat . map (lookupTable tbl)

lookupTable :: Table -> Char -> Hcode
lookupTable [] c = error "lookupTable"
lookupTable ((ch,n):tb) c
  | ch == c   = n
  | otherwise = lookupTable tb c

decodeMessage :: Tree -> Hcode -> [Char]
decodeMessage tr = decodeByt tr
  where
  decodeByt (Node n t1 t2) (L:rest) = decodeByt t1 rest
  decodeByt (Node n t1 t2) (R:rest) = decodeByt t2 rest
  decodeByt (Leaf c n) rest = c : decodeByt tr rest
  decodeByt t [] = []