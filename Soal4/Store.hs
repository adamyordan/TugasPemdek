module Soal4.Store
( Store,
  initial,  -- Store
  value,    -- Store -> Var -> Int
  update    -- Store -> Var -> Int -> Store
) where

newtype Store = Sto [(Int, Var)]
type Var = [Char]

initial :: Store
initial = Sto []

value :: Store -> Var -> Int
value (Sto []) v = 0
value (Sto ((n,w):sto)) v
  | v == w    = n
  | otherwise = value (Sto sto) v

update :: Store -> Var -> Int -> Store
update (Sto sto) v n = Sto ((n,v):sto)