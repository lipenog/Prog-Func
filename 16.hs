funny1 :: Int -> Int -> Int -> Bool
funny1 x y z
    | x > z = True
    | y >= x = False
    | otherwise = True

funny2 :: Int -> Int -> Int -> Bool
funny2 x y z = (x > z)

    