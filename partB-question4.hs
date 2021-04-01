mySplit :: [Int] -> Int -> [([Int], [Int])]

mySplit xs 3 = [splitAt 3 xs]
mySplit xs n  = [splitAt n xs] ++ mySplit xs (n + 1)

split :: [Int] -> [([Int], [Int])]

split xs = mySplit xs 1 