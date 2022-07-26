r6 :: Int -> Float
r6 0 = sqrt 6
r6 x = sqrt (6 + r6 (x - 1))