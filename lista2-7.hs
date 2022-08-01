triangulo :: Int -> Int -> Int -> (String, Int)
triangulo x y z
    | (x == y) && (y == z) = ("Equilatero", (x+y+z))
    | (x == y) || (x == z) || (y == z) = ("Isoceles", (x+y+z))
    | (x <= 0) || (y <= 0) || (z <= 0) = ("Nao e triangulo", 0)
    | otherwise = ("Escaleno", (x+y+z))
