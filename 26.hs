conta :: [Char] -> Char -> Int
conta [] _ = 0
conta (a:x) b
    | a == b = 1 + conta x b
    | otherwise = conta x b