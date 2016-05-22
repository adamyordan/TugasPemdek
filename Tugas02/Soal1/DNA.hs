{-# OPTIONS_GHC -Wall #-}
{-
  Alignment Dua Sequence DNA
  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal1.DNA (showAlign) where

-- Merepresentasikan status pada suatu pasangan karakter
data Edit = Equals |
            Different |
            SpaceFirst |
            SpaceSecond
            deriving (Eq, Show)

-- Mengambil dua sequence (string) sebagai argumen dan memberikan string yang
-- menunjukkan alignment (dengan tampilan seperti di atas) dilengkapi dengan 
-- informasi skor total yang didapat.
showAlign :: String -> String -> String
showAlign xs ys = xs' ++ "\n" ++ ys' ++ "\n" ++ symbols ++ "\n" ++ scores ++ "\n"
                  where
                    edits = (transform xs ys)
                    xs' = modifyFirst  xs edits
                    ys' = modifySecond ys edits
                    symbols = symbolize edits
                    scores  = show (cost edits)

-- Mengambil dua sequence (string) sebagai argumen dan memberikan list
-- status yang merupakan hasil operasi yang memberikan skor terbesar
transform :: String -> String -> [Edit]
transform [] []     = []
transform (_:xs) [] = SpaceSecond : transform xs []
transform [] (_:ys) = SpaceFirst  : transform [] ys
transform (x:xs) (y:ys)
  | x == y    = Equals : transform xs ys
  | otherwise = best  [
                        Different   : transform xs ys,
                        SpaceFirst  : transform (x:xs) ys,
                        SpaceSecond : transform xs (y:ys)
                      ]

-- Mengembalikan list status yang memberikan skor terbesar dari sekumpulan
-- list status yang menjadi argumen
best :: [[Edit]] -> [Edit]
best []  = []
best [x] = x
best (x:xs)
  | cost x >= cost b = x
  | otherwise       = b
  where
    b = best xs

-- Menghitung skor suatu list of status
cost :: [Edit] -> Int
cost [] = 0
cost (x:xs)
  | x == Equals    =  1 + cost xs
  | x == Different = -1 + cost xs
  | otherwise      = -2 + cost xs

-- Mengembalikan sequence pertama setelah modifikasi
modifyFirst :: String -> [Edit] -> String
modifyFirst _ [] = []
modifyFirst [] _ = []
modifyFirst (x:xs) (ed:eds)
  | ed == SpaceFirst = ' ' : modifyFirst (x:xs) eds
  | otherwise        = x   : modifyFirst xs eds

-- Mengembalikan sequence kedua setelah modifikasi
modifySecond :: String -> [Edit] -> String
modifySecond _ [] = []
modifySecond [] _ = []
modifySecond (x:xs) (ed:eds)
  | ed == SpaceSecond = ' ' : modifySecond (x:xs) eds
  | otherwise         = x   : modifySecond xs eds

-- Mengembalikan barisan simbol yang merepresentasikan status
symbolize :: [Edit] -> String
symbolize [] = []
symbolize (ed:eds)
  | ed == Equals    = '+' : symbolize eds
  | ed == Different = '-' : symbolize eds
  | otherwise       = '*' : symbolize eds