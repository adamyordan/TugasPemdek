module Soal3.Frequency (frequency) where

import Soal2.PriorityQueue (PrQueue, build, insert, minElement, extractMin, size)

frequency :: [Char] -> [(Char, Int)]
frequency xs = freqCalc (alphaCalc xs)

alphaCalc :: [Char] -> [(Char, Int)]
alphaCalc xs = alphaDo (build xs) []

alphaDo :: PrQueue Char -> [(Char, Int)] -> [(Char, Int)]
alphaDo pq []
  | size pq == 0 = []
  | otherwise    = [(minElement pq, 1)]
alphaDo pq ((ch, n):xs)
  | size pq == 0        = ((ch, n):xs)
  | minElement pq == ch = alphaDo (fst(extractMin pq)) ((ch, n + 1):xs)
  | otherwise           = alphaDo (fst(extractMin pq)) ((minElement pq, 1):xs)

freqCalc :: [(Char, Int)] -> [(Char, Int)]
freqCalc xs = extractDo (map flipTuple xs)
  where
  	extractDo pq
  	  | size pq == 0 = []
  	  | otherwise    = minElement pq : extractDo (fst (extractMin pq))


flipTuple :: (a, b) -> (b, a)
flipTuple (x,y) = (y,x)