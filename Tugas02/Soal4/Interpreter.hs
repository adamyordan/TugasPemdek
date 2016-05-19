import Soal4.Store

data AritExp = 
     Lit Int |              -- literal bilangan bulat
     Var String |           -- variable dengan nama variable dalam String
     Add AritExp AritExp |  -- penjumlahan
     Mul AritExp AritExp |  -- perkalian
     Sub AritExp AritExp    -- pengurangan

type State = Store

aEval :: AritExp -> State -> Int
aEval (Lit l) _     = l
aEval (Var v) s     = value s v
aEval (Add e1 e2) s = aEval e1 s + aEval e2 s
aEval (Mul e1 e2) s = aEval e1 s * aEval e2 s
aEval (Sub e1 e2) s = aEval e1 s - aEval e2 s


data BoolExp = T |
               F |
               Equal AritExp AritExp |
               Lt  AritExp AritExp    |
               Neg BoolExp           |
               And BoolExp BoolExp   |
               Or BoolExp BoolExp

bEval :: BoolExp -> State -> Bool
bEval T _ = True
bEval F _ = False
bEval (Equal e1 e2) s = aEval e1 s == aEval e2 s
bEval (Lt    e1 e2) s = aEval e1 s <  aEval e2 s
bEval (Neg   e1)    s = not (bEval e1 s)
bEval (And   e1 e2) s = bEval e1 s && bEval e2 s
bEval (Or    e1 e2) s = bEval e1 s || bEval e2 s

data Program = Ass String AritExp |
               Skip |
               Seq Program Program |
               If BoolExp Program Program |
               While BoolExp Program

run :: Program -> State -> State
run (Ass v e) s   = update s v (aEval e s)
run Skip s        = s
run (Seq p1 p2) s = run p2 (run p1 s)
run (If b p1 p2)s = if bEval b s then run p1 s else run p2 s
run (While b p) s = if bEval b s then run (While b p) (run p s) else run Skip s


--test
fac :: Program
fac = Seq (Ass "x" (Lit 5))
 (Seq (Ass "y" (Lit 1))
 (While (Neg (Equal (Var "x") (Lit 0)))
 (Seq (Ass "y" (Mul (Var "x") (Var "y")))
 (Ass "x" (Sub (Var "x") (Lit 1))))))

s0 :: State
s0 = initial

nested :: Program
nested = Seq (Ass "n" (Lit 1))
 (Seq (Ass "sum" (Lit 0))
 (While (Lt (Var "n") (Lit 6))
 (Seq (Ass "x" (Var "n"))
 (Seq (Ass "y" (Lit 1))
 (Seq (While (Neg (Equal (Var "x") (Lit 0)))
 (Seq (Ass "y" (Mul (Var "y") (Var "x")))
 (Ass "x" (Sub (Var "x") (Lit 1)))))
 (Seq (Ass "sum" (Add (Var "sum") (Var "y")))
 (Ass "n" (Add (Var "n") (Lit 1)))))))))