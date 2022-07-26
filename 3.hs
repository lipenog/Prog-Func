soma:: Int -> Int -> Int
soma x y = x + y

mult:: Int -> Int -> Int
mult x 0 = 0
mult 0 y = 0
mult x y = soma x (mult x (y - 1))

