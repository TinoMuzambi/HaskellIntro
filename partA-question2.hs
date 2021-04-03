import Prelude hiding (last)

last :: [Int] -> Int

last xs = head (drop (length xs - 1) xs)
