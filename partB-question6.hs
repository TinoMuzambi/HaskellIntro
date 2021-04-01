data Expr = Val Int 
        | Add Expr Expr 
        | Mul Expr Expr

solve :: [Int] -> Int -> [Expr]

solve [] _ = []