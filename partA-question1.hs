import Prelude hiding (product) 

product :: [Int] -> Int

product [] = 1
product (n: xs) = n * product xs