separaPar :: [Int] -> [Int]
separaPar [] = []
separaPar (a:x)
    | mod a  2 == 0 = a : separaPar x
    | otherwise = separaPar x

separaImPar :: [Int] -> [Int]
separaImPar [] = []
separaImPar (a:x)
    | mod a 2 /= 0 = a : separaImPar x 
    | otherwise = separaImPar x

separa :: [Int] -> ([Int],[Int])
separa [] = ([],[])
separa x = (separaImPar x, separaPar x)

