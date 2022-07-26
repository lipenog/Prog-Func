sales :: Int -> Int
sales 1 = 47
sales 2 = 32
sales 3 = 45
sales 4 = 0
sales 5 = 21
sales 6 = 0
sales 7 = 32
sales _ = 0

--a)
howManyLess :: Int -> Int -> Int -> Int
howManyLess min diaI diaF
    | diaI > diaF = 0
    | sales diaI < min = 1 + howManyLess min (diaI + 1) diaF
    | otherwise = howManyLess min (diaI + 1) diaF
--b)
noZeroPeriod :: Int -> Bool
noZeroPeriod x
    | x == 0 = True
    | sales x == 0 = False
    | otherwise = noZeroPeriod (x - 1)
--c)
zerosPeriod :: Int -> [Int]
zerosPeriod 0 = []
zerosPeriod dia
    | sales dia == 0 = dia : zerosPeriod (dia - 1)
    | otherwise = zerosPeriod (dia - 1)
--d)
salesLessThen :: Int -> Int -> [Int]
salesLessThen _ 0 = []
salesLessThen min dia
    | sales dia < min = dia : salesLessThen min (dia - 1)
    | otherwise = salesLessThen min (dia - 1)