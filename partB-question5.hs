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
exprs xs = [e | (ls,rs) <- split xs
                , x <- exprs ls
                , y <- exprs rs
                , e <- combine x y]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o x y | o <- [Add,Mul]]