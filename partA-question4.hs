safetail :: [a] -> [a]

safetail xs = if null xs
                    then []
                else tail xs

safetail2 :: [a] -> [a]

safetail2 xs = [x | x <- tail xs, not (null xs)]

safetail3 :: [a] -> [a]

safetail3 [] = []
safetail3 xs = tail xs