f1 :: Int -> Int
f1 0 = 0
f1 x = x*x + f1 (x - 1)

f2 :: Int -> a -> [a]
f2 0 _ = []
f2 n s = s : f2 (n - 1) s

f7 :: [Int] -> [Int] -> Int
f7 [] [] = 0
f7 (a:x) (b:y) = a*b + f7 x y

tam :: [Int] -> Int
tam [] = 0
tam (a:x) = 1 + tam x

f10 :: [Int] -> Int
f10 [] = 0
f10 (a:x) = a*10^(tam x) + f10 x
