module Soal1.DNA (showAlign) where

data Edit = Equals |
            Different |
            SpaceFirst |
            SpaceSecond
            deriving (Eq, Show)

transform :: String -> String -> [Edit]
transform [] [] = []
transform (x:xs) [] = SpaceSecond : transform xs []
transform [] (y:ys) = SpaceFirst  : transform [] ys
transform (x:xs) (y:ys)
  | x == y    = Equals : transform xs ys
  | otherwise = best  [
                        Different   : transform xs ys,
                        SpaceFirst  : transform (x:xs) ys,
                        SpaceSecond : transform xs (y:ys)
                      ]

best :: [[Edit]] -> [Edit]
best []  = []
best [x] = x
best (x:xs)
  | cost x > cost b = x
  | otherwise       = b
  where
    b = best xs

cost :: [Edit] -> Int
cost [] = 0
cost (x:xs)
  | x == Equals    =  1 + cost xs
  | x == Different = -1 + cost xs
  | otherwise      = -2 + cost xs


showAlign :: String -> String -> String
showAlign xs ys = xs' ++ "\n" ++ ys' ++ "\n" ++ symbols ++ "\n" ++ scores ++ "\n"
                  where
                    edits = (transform xs ys)
                    xs' = modifyFirst  xs edits
                    ys' = modifySecond ys edits
                    symbols = symbolize edits
                    scores  = show (cost edits)

modifyFirst :: String -> [Edit] -> String
modifyFirst _ [] = []
modifyFirst [] _ = []
modifyFirst (x:xs) (ed:eds)
  | ed == SpaceFirst = ' ' : modifyFirst (x:xs) eds
  | otherwise        = x   : modifyFirst xs eds

modifySecond :: String -> [Edit] -> String
modifySecond _ [] = []
modifySecond [] _ = []
modifySecond (x:xs) (ed:eds)
  | ed == SpaceSecond = ' ' : modifySecond (x:xs) eds
  | otherwise         = x   : modifySecond xs eds

symbolize :: [Edit] -> String
symbolize [] = []
symbolize (ed:eds)
  | ed == Equals    = '+' : symbolize eds
  | ed == Different = '-' : symbolize eds
  | otherwise       = '*' : symbolize eds