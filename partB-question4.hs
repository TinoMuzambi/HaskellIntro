mySplit :: [Int] -> Int -> Bool -> [([Int], [Int])]

mySplit xs n False = [splitAt 3 xs]
mySplit xs n True  = [splitAt n xs] ++ mySplit xs (n + 1) True

split :: [Int] -> [([Int], [Int])]

split xs = mySplit xs 1 True