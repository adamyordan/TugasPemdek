{-# OPTIONS_GHC -Wall #-}
{-
  Interpreter Program Imperative Sederhana
  sebuah prototipe untuk interpreter yang mampu menangani proses variable 
  assignment, evaluasi ekspresi aritmatika dan boolean, serta
  melakukan operasi loop dengan menggunakan “while”.

  Nama  : Adam Jordan
  NPM   : 1406567536
  Kelas : C
-}
module Soal4.Interpreter (Program, AritExp, BoolExp, run) where

import Soal4.Store (Store, value, update)

-- Tipe Data Ekspresi Aritmetika 
data AritExp = 
     Lit Int |              -- literal bilangan bulat
     Var String |           -- variable dengan nama variable dalam String
     Add AritExp AritExp |  -- penjumlahan
     Mul AritExp AritExp |  -- perkalian
     Sub AritExp AritExp    -- pengurangan

type State = Store

-- Menerima suatu ekspresi aritmetika dan State sebagai argumen,
-- kemudian mengembalikan nilai hasil evaluasi ekspresi aritmetika tersebut
aEval :: AritExp -> State -> Int
aEval (Lit l) _     = l
aEval (Var v) s     = value s v
aEval (Add e1 e2) s = aEval e1 s + aEval e2 s
aEval (Mul e1 e2) s = aEval e1 s * aEval e2 s
aEval (Sub e1 e2) s = aEval e1 s - aEval e2 s

-- Tipe Data Ekspresi Boolean
data BoolExp = T |                       -- True
               F |                       -- False
               Equal AritExp AritExp |   -- equality of arithmetic expressions
               Lt  AritExp AritExp   |   -- less than, E1 < E2, of arithmetic exp.
               Neg BoolExp           |   -- negation
               And BoolExp BoolExp   |   -- and
               Or BoolExp BoolExp        -- or

-- Menerima suatu ekspresi boolean dan State sebagai argumen,
-- kemudian mengembalikan nilai hasil evaluasi ekspresi boolean tersebut
bEval :: BoolExp -> State -> Bool
bEval T _ = True
bEval F _ = False
bEval (Equal e1 e2) s = aEval e1 s == aEval e2 s
bEval (Lt    e1 e2) s = aEval e1 s <  aEval e2 s
bEval (Neg   e1)    s = not (bEval e1 s)
bEval (And   e1 e2) s = bEval e1 s && bEval e2 s
bEval (Or    e1 e2) s = bEval e1 s || bEval e2 s

-- Tipe Data Program Statements
data Program = Ass String AritExp |         -- assignment sebuah variable
               Skip |                       -- statement kosong (do nothing)
               Seq Program Program |        -- sequential statement composition
               If BoolExp Program Program | -- if b then p1 else p2
               While BoolExp Program        -- while

-- Menerima suatu Program Statement dan State sebagai argumen,
-- kemudian mengembalikan suatu state hasil modifikasi dari menjalankan
-- program statement tersebut.
run :: Program -> State -> State
run (Ass v e) s   = update s v (aEval e s)
run Skip s        = s
run (Seq p1 p2) s = run p2 (run p1 s)
run (If b p1 p2)s = if bEval b s then run p1 s else run p2 s
run (While b p) s = if bEval b s then run (While b p) (run p s) else run Skip s