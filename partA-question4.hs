safetail :: [a] -> [a]

safetail xs = if null xs
                    then []
                else tail xs

safetail3 :: [a] -> [a]

safetail3 [] = []
safetail3 xs = tail xs