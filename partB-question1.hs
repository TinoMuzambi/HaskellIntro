data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

values :: Expr -> [Int]
values (Val n) = [n]
values (Add x y) = values x ++ values y 
values (Mul x y) = values x ++ values y 