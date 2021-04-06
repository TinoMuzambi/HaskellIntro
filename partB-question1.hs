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