{-# OPTIONS_GHC -Wall #-}
{-
  Minimum Priority Queue
  Module PriorityQueue yang direpresentasikan dengan menggunakan
  Binary Heap

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal2.PriorityQueue
( PrQueue,
  build,       -- Ord a => [a] -> PrQueue a 
  insert,      -- Ord a => PrQueue a -> a -> PrQueue a
  minElement,  -- PrQueue a -> a
  extractMin,  -- Ord a => PrQueue a -> (PrQueue a,a)
  size         -- PrQueue a -> Int 
) where

-- Tipe data PriorityQueue
newtype PrQueue a = PrQ [a] deriving Show

-- Membentuk suatu PriorityQueue dari list elemen
build :: Ord a => [a] -> PrQueue a
build []     = PrQ []
build (x:xs) = insert (build xs) x

-- Menambahkan suatu elemen ke dalam sebuah PriorityQueue
insert :: Ord a => PrQueue a -> a -> PrQueue a
insert (PrQ []) x = PrQ [x]
insert (PrQ xs) x = percolateUp (PrQ (xs ++ [x])) (length xs)

-- Mengembalikan elemen terkecil dari sebuah PriorityQueue
minElement :: PrQueue a -> a
minElement (PrQ [])     = error "heap underflow"
minElement (PrQ (x:_)) = x

-- Mengembalikan tuple yang berisi dua elemen:
-- elemen pertama merupakan PriorityQueue setelah elemen terkecil diambil
-- elemen kedua merupakan elemen terkecil PriorityQueue
extractMin :: Ord a => PrQueue a -> (PrQueue a, a)
extractMin (PrQ xs) = (removeMin (PrQ xs), minElement (PrQ xs))

-- Mengembalikan banyaknya elemen dalam suatu PriorityQueue
size :: PrQueue a -> Int
size (PrQ xs) = length xs

-- Mengembalikan indeks parent dari suatu indeks node binary heap
parent :: Int -> Int
parent n = ((n + 1) `div` 2) - 1

-- Mengembalikan indeks anak kiri dari suatu indeks node binary heap
leftChild :: Int -> Int
leftChild n = (2 * (n + 1)) - 1

-- Mengembalikan indeks anak kanan dari suatu indeks node binary heap
rightChild :: Int -> Int
rightChild n = (2 * (n + 1) + 1) - 1

-- Operasi Percolate Up yang digunakan untuk menjaga karakteristik
-- dari Binary Heap yang dijalankan secara rekursif
-- dari suatu node ke parentnya
percolateUp :: Ord a => PrQueue a -> Int -> PrQueue a
percolateUp pq 0 = pq
percolateUp (PrQ xs) n
    | xs !! n < xs !! (parent n) = percolateUp (PrQ (swap xs n (parent n))) (parent n)
    | otherwise                  = PrQ xs

-- Operasi Percolate Down yang digunakan untuk menjaga karakteristik
-- dari Binary Heap yang dijalankan secara rekursif
-- dari suatu node ke anak
percolateDown :: Ord a => PrQueue a -> Int -> PrQueue a
percolateDown (PrQ xs) n
    | minIndex == n = PrQ xs
    | otherwise     = percolateDown (PrQ (swap xs n minIndex)) minIndex
    where
      minIndex = minIndexAgainstChild (PrQ xs) n

-- Menerima argumen sebuah PriorityQueue dan mengembalikan
-- PriorityQueue setelah elemen terkecilnya dihapus
removeMin :: Ord a => PrQueue a -> PrQueue a
removeMin (PrQ [])  = error "heap underflow"
removeMin (PrQ [_]) = PrQ []
removeMin (PrQ xs)  = percolateDown (PrQ (last xs : tail(init xs))) 0

-- Mengembalikan sebuah indeks dari antara suatu node dengan anak-anaknya
-- Indeks yang dikembalikan adalah indeks dari node yang memiliki
-- elemen dengan nilai terkecil
minIndexAgainstChild :: Ord a => PrQueue a -> Int -> Int
minIndexAgainstChild (PrQ xs) n
    | leftChild n  >= length xs = n
    | rightChild n >= length xs = if (xs !! n) <= (xs !! leftChild n) 
                                    then n 
                                    else leftChild n
    | (xs !! rightChild n) < (xs !! leftChild n) = if (xs !! n) <= (xs !! rightChild n) 
                                                     then n 
                                                     else rightChild n
    | otherwise = if (xs !! n) <= (xs !! leftChild n) 
                    then n
                    else leftChild n

-- Menukar dua buah elemen dari suatu list
swap :: [a] -> Int -> Int -> [a]
swap xs m n 
  | m > length xs || n > length xs = error "out of bound"  
  | m == n    = xs
  | m > n     = swap xs n m    
  | otherwise = partBeforeM ++ [elemN] ++ partBetweenMN ++ [elemM] ++ partAfterN
    where
      partBeforeM   = take m xs
      elemN         = xs !! n
      partBetweenMN = drop (m+1) (take n xs)
      elemM         = xs !! m
      partAfterN    = drop (n+1) xs