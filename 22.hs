inverte :: [Int] -> [Int]
inverte [] = []
inverte (a:x) = inverte x ++ [a]