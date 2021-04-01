mySplit :: [Int] -> Int -> [([Int], [Int])]

mySplit xs n  = [splitAt n xs]

split :: [Int] -> [([Int], [Int])]

split xs = mySplit xs 1 