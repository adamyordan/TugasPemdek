module Soal2.PriorityQueue
( PrQueue,
  build,
  insert,
  minElement,
  extractMin,
  size
) where

newtype PrQueue a = PrQ [a] deriving Show

build :: Ord a => [a] -> PrQueue a
build [] = PrQ []
build (x:xs) = insert (build xs) x

insert :: Ord a => PrQueue a -> a -> PrQueue a
insert (PrQ []) x = PrQ [x]
insert (PrQ xs) x = percolateUp (PrQ (xs ++ [x])) (length xs)

minElement :: PrQueue a -> a
minElement (PrQ []) = error "heap underflow"
minElement (PrQ (x:xs)) = x

extractMin :: Ord a => PrQueue a -> (PrQueue a, a)
extractMin (PrQ xs) = (removeMin (PrQ xs), minElement (PrQ xs))

size :: PrQueue a -> Int
size (PrQ xs) = length xs


parent :: Int -> Int
parent n = n `div` 2

leftChild :: Int -> Int
leftChild n = 2 * n + 1

rightChild :: Int -> Int
rightChild n = 2 * n

percolateUp :: Ord a => PrQueue a -> Int -> PrQueue a
percolateUp pq 0 = pq
percolateUp (PrQ xs) n
    | xs !! n < xs !! (parent n) = percolateUp (PrQ (swap xs n (parent n))) (parent n)
    | otherwise                  = PrQ xs

percolateDown :: Ord a => PrQueue a -> Int -> PrQueue a
percolateDown (PrQ xs) n
    | minIndex == n = PrQ xs
    | otherwise     = percolateDown (PrQ (swap xs n minIndex)) minIndex
    where
      minIndex = minIndexAgainstChild (PrQ xs) n

removeMin :: Ord a => PrQueue a -> PrQueue a
removeMin (PrQ xs) = percolateDown (PrQ (last xs : tail(init xs))) 0

minIndexAgainstChild :: Ord a => PrQueue a -> Int -> Int
minIndexAgainstChild (PrQ xs) n
    | leftChild n  >= length xs = n
    | rightChild n >= length xs = if (xs !! n) < (xs !! rightChild n) 
                                    then n 
                                    else rightChild n
    | (xs !! rightChild n) > (xs !! leftChild n) = if (xs !! n) < (xs !! rightChild n) 
                                                     then n 
                                                     else rightChild n
    | otherwise = if (xs !! n) < (xs !! leftChild n) 
                    then n
                    else leftChild n


swap :: [a] -> Int -> Int -> [a]
swap xs m n 
  | m > length xs || n > length xs = error "out of bound"  
  | m == n    = xs
  | m > n     = swap xs n m    
  | otherwise = take (parent n) front 
                ++ [front !! n] 
                ++ tail(init(drop (parent n) front)) 
                ++ [front !! (parent n)]
                ++ back
                where
                    (front, back) = splitAt (n+1) xs