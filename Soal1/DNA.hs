module Soal1.DNA (showAlign) where

transform :: String -> String -> (String, String)
transform xs "" = (xs, "")
transform "" ys = ("", ys)
transform (x:xs) (y:ys)
  | x == y    = (x: fst (transform xs ys), y : snd (transform xs ys))
  | length xs < length ys = best [
                        (  x : fst (transform xs ys)    ,   y : snd (transform xs ys)),
                        (' ' : fst (transform (x:xs) ys),   y : snd (transform (x:xs) ys))
                      ]
  | length xs > length ys = best [
                        (  x : fst (transform xs ys)    ,   y : snd (transform xs ys)),
                        (  x : fst (transform (x:xs) ys), ' ' : snd (transform (x:xs) ys))
                     ]
  | otherwise = best [
                        (  x : fst (transform xs ys)    ,   y : snd (transform xs ys)),
                        (' ' : fst (transform (x:xs) ys),   y : snd (transform (x:xs) ys)),
                        (  x : fst (transform xs (y:ys)), ' ' : snd (transform xs (y:ys)))
                     ]

best :: [(String, String)] -> (String, String)
best []  = ""
best [x] = x
best ((x1, x2):xs)
  | score x1 x2 >= score b1 b2 = (x1, x2)
  | otherwise = (b1, b2)
    where
      (b1, b2) = best xs

score :: String -> String -> Int
score xs "" = -2 * length xs
score "" ys = -2 * length ys
score (x:xs) (y:ys)
  | x == ' ' || y == ' '  = -2 + score xs ys
  | x == y                =  1 + score xs ys
  | otherwise             = -1 + score xs ys

symbolize :: String -> String -> String
symbolize xs "" = replicate (length xs) '*'
symbolize "" ys = replicate (length ys) '*'
symbolize (x:xs) (y:ys)
  | x == ' ' || y == ' ' = '*' : symbolize xs ys
  | x == y               = '+' : symbolize xs ys
  | otherwise            = '-' : symbolize xs ys

showAlign :: String -> String -> String
showAlign xs ys = xs' ++ "\n" ++ ys' ++ "\n" ++ symbols ++ "\n" ++ scores ++ "\n"
                  where
                    (xs', ys') = transform xs ys
                    symbols = symbolize xs' ys'
                    scores = show (score xs' ys')
