data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y

eval :: Expr -> Int
eval (Val n) = n
eval (App o x y) = apply o (eval x) (eval y)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ x y) = values x ++ values y

delete :: Int -> [Int] -> [Int]

delete n [] = []
delete n (x:xs)
    | (n == x) = xs
    | otherwise = x : delete n xs

perms :: [Int] -> [[Int]]

perms [] = [[]]
perms xs = do { x <- xs
            ; let l = delete x xs
            ; ls <- perms l
            ; return $ x : ls }


mySplit :: [Int] -> Int -> Bool -> [([Int], [Int])]

mySplit xs n False = [splitAt n xs]
mySplit xs n True  = if n == (length xs - 2)
                        then [splitAt n xs] ++ mySplit xs (n + 1) False
                    else [splitAt n xs] ++ mySplit xs (n + 1) True

split :: [Int] -> [([Int], [Int])]

split xs = if 2 == (length xs)
            then mySplit xs 1 False
        else mySplit xs 1 True

-- combine :: Expr -> Expr -> [Expr]
-- combine l r = [App o x y | o <- [Add,Mul]]

-- exprs :: [Int] -> [Expr]
-- exprs [] = []
-- exprs [n] = [Val n]
-- exprs xs = [e | (ls,rs) <- split xs
--                 , x <- exprs ls
--                 , y <- exprs rs
--                 , e <- combine x y]


-- solve :: [Int] -> Int -> [Expr]
-- solve xs x = [e | xs' <- choices xs
--                 , e <- exprs xs'
--                 , eval e == [x]]