data Expr = Val Int 
        | Add Expr Expr 
        | Mul Expr Expr

solve :: [Int] -> Int -> [Expr]
solve ns n = [e | ns' <- choices ns
                , e <- exprs ns'
                , eval e == [n]]