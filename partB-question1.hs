data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y

eval :: Expr -> Int
eval (Val n) = n
eval (App o left right) = apply o (eval left) (eval right)

values :: Expr -> [Int]
values (Val n) = [n]
-- values (Add x y) = values x ++ values y 
-- values (Mul x y) = values x ++ values y 