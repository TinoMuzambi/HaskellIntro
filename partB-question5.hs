mySplit :: [Int] -> Int -> Bool -> [([Int], [Int])]

mySplit xs n False = [splitAt n xs]
mySplit xs n True  = if n == (length xs - 2)
                        then [splitAt n xs] ++ mySplit xs (n + 1) False
                    else [splitAt n xs] ++ mySplit xs (n + 1) True

split :: [Int] -> [([Int], [Int])]

split xs = if 2 == (length xs)
            then mySplit xs 1 False
        else mySplit xs 1 True

data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
                , l <- exprs ls
                , r <- exprs rs
                , e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add,Mul]]