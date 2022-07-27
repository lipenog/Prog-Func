verifica :: Int -> [Int] -> Bool
verifica b [] = False
verifica b (a:x)
    | b == a = True
    | otherwise = verifica b x


purifica :: [Int] -> [Int]
purifica [] = []
purifica (a:x)
    | verifica a x = purifica x
    | otherwise = a : purifica x