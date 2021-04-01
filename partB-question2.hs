-- delete n xs = [x | x <- xs, n /= x]

delete n [] = []
delete n (x:xs)
    | (n == x) = xs
    | otherwise = x : delete n xs