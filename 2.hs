fat:: Int -> Int
fat x
    | x < 0 = -1
    | x == 0 = 1
    | otherwise = x * fat (x-1)