product :: [Int] -> Int
product [] = 1
product (n: xs) = n * Main.product xs