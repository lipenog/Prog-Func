square :: Int -> Int
square 0 = 0
square 1 = 1
square x = x * x


--Não entendi como fazer recursivo
fourP :: Int -> Int
fourP 0 = 0
fourP 1 = 1
fourP x = square x * square x 