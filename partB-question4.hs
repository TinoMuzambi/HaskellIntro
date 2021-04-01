mySplit :: [Int] -> Int -> Bool -> [([Int], [Int])]

mySplit xs n False = [splitAt n xs]
mySplit xs n True  = if n == (length xs - 2)
                        then [splitAt n xs] ++ mySplit xs (n + 1) False
                    else [splitAt n xs] ++ mySplit xs (n + 1) True

split :: [Int] -> [([Int], [Int])]

split xs = if 2 == (length xs)
            then mySplit xs 1 False
        else mySplit xs 1 True