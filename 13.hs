howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | ((a == b)  && (b == c)) = 3
    | ((a == b)  || (b == c)) = 2
    | otherwise = 0