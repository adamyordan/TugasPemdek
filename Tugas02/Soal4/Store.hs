{-# OPTIONS_GHC -Wall #-}
{-
  Store

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal4.Store
( Store,
  initial,  -- Store
  value,    -- Store -> Var -> Int
  update    -- Store -> Var -> Int -> Store
) where

-- Type sinonym untuk Var adalah String
type Var = [Char]

-- Tipe Data Store
newtype Store = Sto [(Int, Var)]

-- Menginisiasi suatu Store kosong
initial :: Store
initial = Sto []

-- Mengembalikan nilai dari suatu variabel dalam Store
value :: Store -> Var -> Int
value (Sto []) _ = 0
value (Sto ((n,w):sto)) v
  | v == w    = n
  | otherwise = value (Sto sto) v

-- Mengisi atau mengupdate nilai dari suatu 
-- variabel dalam Store
update :: Store -> Var -> Int -> Store
update (Sto sto) v n = Sto ((n,v):sto)