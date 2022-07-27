listaRepete :: Int -> Int -> [Int]
listaRepete 0 _ = []
listaRepete _ 0 = []
listaRepete a b = a : listaRepete a (b - 1)

proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (a:x) = listaRepete a a ++ proliferaInt x