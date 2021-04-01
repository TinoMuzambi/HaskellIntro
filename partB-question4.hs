mySplit :: Int -> [Int] -> [([Int], [Int])]

mySplit n xs = [splitAt n xs]

split :: [Int] -> [([Int], [Int])]

split xs = mySplit (length xs) xs