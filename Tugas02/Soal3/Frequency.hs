{-# OPTIONS_GHC -Wall #-}
{-
  Huffman Code - Module Frequency
  Berisikan fungsi yang menerima input sebuah string dan
  mengembalikan sebuah list yang berisi karakter dan frekuensinya
  secara terurut menaik berdasarkan frekuensinya

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal3.Frequency (frequency) where

import Soal2.PriorityQueue (PrQueue, build, minElement, extractMin, size)

-- Tipe data yang berisikan karakter dan frekuensinya
data CharFreq = CF Char Int deriving Eq
instance Ord CharFreq where
  compare (CF _ n1) (CF _ n2) = compare n1 n2

-- Menerima input berupa String, lalu mengembalikan list dari tuple yang 
-- berisi karakter dan frekuensi kemunculannya. Tuple dikembalikan dalam 
-- kondisi terurut dari jumlah kemunculannya
frequency :: [Char] -> [(Char, Int)]
frequency xs = sortByFreq (calcFreq (heapSort xs))

-- Menerima input berupa String, lalu mengembalikan list tuple yang
-- berisi karakter dan frekuensi kemunculannya
calcFreq :: [Char] -> [(Char, Int)]
calcFreq xs = calcFreqHelper xs []

-- Fungsi helper untuk fungsi calcFreq
calcFreqHelper :: [Char] -> [(Char, Int)] -> [(Char, Int)]
calcFreqHelper [] ys = ys
calcFreqHelper (x:xs) ys = calcFreqHelper xs (insertChar x ys)

-- Menambahkan karakter ke sebuah list yang berisi frekuensi karakter
insertChar :: Char -> [(Char, Int)] -> [(Char, Int)]
insertChar c [] = [(c,1)]
insertChar c ((c2,n):xs)
  | c == c2   = (c2, n+1) : xs
  | otherwise = (c, 1) : (c2, n) : xs

-- Mengurutkan frekuensi karakter berdasarkan jumlah kemunculannya
sortByFreq :: [(Char, Int)] -> [(Char, Int)]
sortByFreq xs = map decapsulate (heapSort (map encapsulate xs))
  where
    encapsulate :: (Char, Int) -> CharFreq  
    encapsulate (x,y) = CF x y
    decapsulate :: CharFreq -> (Char, Int)
    decapsulate (CF x y) = (x,y)

-- Operasi sorting dengan menggunakan algoritma heap sort
heapSort :: Ord a => [a] -> [a]
heapSort xs = heapSortHelper (build xs) []

-- Fungsi helper untuk fungsi heapSort
heapSortHelper :: Ord a => PrQueue a -> [a] -> [a]
heapSortHelper pq xs
  | size pq == 0 = xs
  | otherwise    = heapSortHelper (fst (extractMin pq)) (xs ++ [minElement pq])
