repeteChar Char -> Int -> [Char]
repeteChar _ 0 = []
repeteChar a b = a : repeteChar a (b - 1)

proliferaChar :: [Char] -> [Char]