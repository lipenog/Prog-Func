import Data.Char

f2 :: Int -> Int
f2 x 
    | x <= 0 = 1
    | otherwise = x * f2 (x - 1)

f3 :: Int -> Int -> Int
f3 _ 0 = 0
f3 0 _ = 0
f3 x y = x + f3 x (y - 1)

tamInt :: Int -> Int
tamInt 0 = 1
tamInt x = 1 + tamInt (div x 10)

f11 :: Int -> Int -> Int
f11 numero x
    | x < tamInt numero =  mod (div numero (10^((tamInt numero) - x - 2))) 10
    | otherwise = (-1)

sales :: Int -> Int
sales 1 = 42
sales 2 = 0
sales 3 = 0
sales 4 = 10
sales 5 = 0
sales 6 = 50
sales 7 = 30
sales _ = 0

f14a :: Int -> Int -> Int -> Int
f14a min inicio fim
    | inicio > fim = 0
    | sales inicio < min = 1 + f14a min (inicio + 1) fim
    | otherwise = f14a min (inicio + 1) fim

f14b :: Int -> Bool
f14b 1 = not (sales 1 == 0)
f14b x = not (sales x == 0) && f14b (x - 1)

f14c :: Int -> [Int]
f14c 0 = []
f14c x 
    | sales x == 0 = x : f14c (x - 1)
    | otherwise = f14c (x - 1)

f14d :: Int -> Int -> [Int]
f14d _ 0 = []
f14d valor dia
    | sales dia < valor = dia : f14d valor (dia - 1)
    | otherwise = f14d valor (dia - 1)


f19 :: String -> Int -> String
f19 _ 0 = ""
f19 s n = s ++ f19 s (n - 1)

tam :: String -> Int
tam [] = 0
tam (a:x) = 1 + tam x

f20 :: String -> Int -> String
f20 s n
    | n <= tam s = s
    | otherwise = ">" ++ f20 s (n - 1)

f22 :: [Int] -> [Int]
f22 [] = []
f22 (a:x) = f22 x ++ [a]

par :: Int -> Bool
par x = mod x 2 == 0

impar :: Int -> Bool
impar x = not (mod x 2 == 0)

f23a :: [Int] -> (Int -> Bool) -> [Int]
f23a [] _ = []
f23a (a:x) f
    | f a = a : f23a x f
    | otherwise = f23a x f


f23 :: [Int] -> ([Int],[Int])
f23 x = (f23a x impar, f23a x par)

f26 :: String -> Char -> Int
f26 [] x = 0
f26 (a:x) b
    | a == b = 1 + f26 x b
    | otherwise = f26 x b

converte::[Int]->[Char]
converte [] = ""
converte (a:b) = chr (a+64) : converte b

cabeca :: [Int] -> Int -> Bool
cabeca [] _ = True 
cabeca (a:x) b = not(a == b)

f27 :: [Int] -> [Int]
f27 [] = []
f27 (a:x)
    | cabeca x a = a : f27 x
    | otherwise = f27 x

repete :: Int -> Int -> [Int]
repete 0 _ = []
repete _ 0 = []
repete valor n = valor : repete valor (n - 1)

f28 :: [Int] -> [Int]
f28 [] = []
f28 (a:x) = repete a a ++ f28 x


qSort :: [Int] -> [Int]
qSort [] = []
qSort (a:x) = qSort (particionaMenor a x) ++ [a] ++ qSort (particionaMaior a x)

particionaMaior :: Int -> [Int] -> [Int]
particionaMaior _ [] = []
particionaMaior pivo (a:x)
    | a >= pivo = a : particionaMaior pivo x
    | otherwise = particionaMaior pivo x

particionaMenor :: Int -> [Int] -> [Int]
particionaMenor _ [] = []
particionaMenor pivo (a:x)
    | a < pivo = a : particionaMenor pivo x
    | otherwise = particionaMenor pivo x
