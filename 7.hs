escolhe :: Int -> Int -> Int
escolhe n m 
    | n == 1 = m
    | n == 0 = 0
    | otherwise = m * escolhe (n - 1) (m - 1)