delete n [] = []
delete n (x:xs)
    | (n == x) = xs
    | otherwise = x : delete n xs