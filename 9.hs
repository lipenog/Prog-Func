howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples n a b
    | a > b = 0
    | (mod a n) == 0 = 1 + howManyMultiples n (a + 1) b
    | otherwise = howManyMultiples n (a + 1) b