base :: Int -> (Int, String, String, Char)
base x
    |x == 0 = (1793, "Pedro Paulo" ,"MESTRE", 'M')
    |x == 1 = (1797, "Joana Silva Alencar","MESTRE", 'M')
    |x == 2 = (1534, "João De Medeiros" ,"DOUTOR", 'F')
    |x == 3 = (1267, "Cláudio César de Sá","DOUTOR", 'M')
    |x == 4 = (1737, "Paula de Medeiros" ,"MESTRE", 'F')
    |x == 5 = (1888, "Rita de Matos","MESTRE", 'F')
    |x == 9 = (1698, "Tereza Cristina Andrade" ,"MESTRE", 'F')
    |x == 10 = (0, "", "", '0')

-- A)
tituloI :: (Int, String, String, Char) -> String
tituloI (_,_,t,_) = t

titulo :: Int -> String
titulo x = tituloI (base x)

-- Como fazer no caso de 9 para pular direto para o 5 ???
doutores :: Int -> Int
doutores (-1) = 0
doutores x
    | titulo x == "DOUTOR" = 1 + doutores (x - 1)
    | otherwise = doutores (x - 1)

-- B)
sexoI :: (Int, String, String, Char) -> Char
sexoI (_,_,_,s) = s

sexo :: Int -> Char
sexo x = sexoI (base x)

mulheres :: Int -> Int
mulheres (-1) = 0
mulheres x 
    | sexo x == 'F' = 1 + mulheres (x - 1)
    | otherwise = mulheres (x - 1)

-- C)
mestresH :: Int -> Int
mestresH (-1) = 0
mestresH x
    | (sexo x == 'M') && (titulo x == "MESTRE") = 1 + mestresH (x - 1)
    | otherwise = mestresH (x - 1)

-- D)
matriI :: (Int, String, String, Char) -> Int
matriI (m,_,_,_) = m

matricula :: Int -> Int
matricula x = matriI (base x)

nomeI :: (Int, String, String, Char) -> String
nomeI (_,n,_,_) = n

nome :: Int -> String
nome x = nomeI (base x)

profAnt :: Int -> Int -> String
profAnt baseMaior 0 = nome baseMaior
profAnt baseMaior registros
    | matricula registros < matricula baseMaior = profAnt registros (registros - 1)
    | otherwise = profAnt baseMaior (registros - 1)

professorMaisAntigo :: Int -> String
professorMaisAntigo x = profAnt x x
