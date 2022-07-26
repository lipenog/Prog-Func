f1:: Float -> Float
f1 x
    | x >= 0 = (x+4)/(x+2)
    | otherwise = 2/x

f2:: Int -> Int -> Int
f2 x y
    | x >= y = x + y
    | otherwise = x - y

f3:: Int -> Int -> Int -> Int
f3 x y z
    | (x + y) > z = x + y + z
    | (x + y) < z = x - y - z
    | otherwise = 0
