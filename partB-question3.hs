-- delete :: Int -> [Int] -> [Int]

-- delete n [] = []
-- delete n (x:xs)
--     | (n == x) = xs
--     | otherwise = x : delete n xs
import partB-question2

perms :: [Int] -> [[Int]]

perms [] = [[]]
perms xs = do { x <- xs
            ; let l = delete x xs
            ; ls <- perms l
            ; return $ x : ls }