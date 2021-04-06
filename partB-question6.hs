data Expr = Val Int 
        | Add Expr Expr 
        | Mul Expr Expr

solve :: [Int] -> Int -> [Expr]
solve xs x = [e | xs' <- choices xs
                , e <- exprs xs'
                , eval e == [x]]