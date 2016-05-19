module Soal3.CodeTable (codeTable) where

import Soal3.Types

convert :: HCode -> Tree -> Table
convert cd (leaf c n) = [(c,cd)]
convert cd (Node n t1 t2) = (convert (cd++[L]) t1) ++ (convert (cd++[R]) t2)

codeTable :: Tree -> Table
codeTable = convert []