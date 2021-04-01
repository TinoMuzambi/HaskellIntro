data Expr = Val Int 
        | Add Expr Expr 
        | Mul Expr Expr

exprs :: [Int] -> [Expr]

exprs [] = []