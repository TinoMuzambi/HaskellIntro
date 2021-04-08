-- Question 1
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

-- Question 2
delete :: Int -> [Int] -> [Int]

delete n [] = []
delete n (x:xs)
    | (n == x) = xs
    | otherwise = x : delete n xs

-- Question 3
perms :: [Int] -> [[Int]]

perms [] = [[]]
perms xs = [ x : ls | x <- xs
               , let l = delete x xs
               , ls <- perms l]


-- Question 4
mySplit :: [Int] -> Int -> Bool -> [([Int], [Int])]

mySplit xs n False = [splitAt n xs]
mySplit xs n True  = if n == (length xs - 2)
                        then [splitAt n xs] ++ mySplit xs (n + 1) False
                    else [splitAt n xs] ++ mySplit xs (n + 1) True

split :: [Int] -> [([Int], [Int])]

split xs = if (length xs) == 2
            then mySplit xs 1 False
        else mySplit xs 1 True

-- Question 5
combine :: Expr -> Expr -> [Expr]
combine x y = [App o x y | o <- [Add,Mul]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [l | (ls,rs) <- split xs
                , x <- exprs ls
                , y <- exprs rs
                , l <- combine x y]


-- Question 6
-- solve :: [Int] -> Int -> [Expr]
-- solve xs x = [l | xs' <- choices xs
--                 , l <- exprs xs'
--                 , eval l == [x]]

combinations :: Int -> [a] -> [[a]]

combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

choices :: [a] -> [[a]]
