import Data.Char

pessoa :: Int -> ([Char], Int, Char)
pessoa rg 
    | rg == 1 = ("Joao Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza", 51, 'm')
    | rg == 3 = ("Cleide", 22, 'f')
    | rg == 4 = ("Julia", 33, 'f')
    | rg == 5 = ("andre", 21, 'm')
    | rg == 6 = ("amora", 45, 'f')
    | rg == 7 = ("Juliana", 11, 'f')
    | otherwise = ("Nao ha ninguem mais", 9999, 'x')

-- A)
idade :: ([Char], Int, Char) -> Int
idade (_,i,_) = i

nome :: ([Char], Int, Char) -> [Char]
nome (n,_,_) = n

menorIdade :: Int -> Int -> Int
menorIdade menorRg 0 = menorRg
menorIdade menorRg rg
    | idade (pessoa rg) < idade (pessoa menorRg) = menorIdade rg (rg - 1)
    | otherwise = menorIdade menorRg (rg - 1)

menorIdadeTotal :: Int -> [Char]
menorIdadeTotal x = nome (pessoa (menorIdade (idade (pessoa x)) x))

-- B)
somaIdades :: Int -> Int
somaIdades 0 = 0
somaIdades rg = idade (pessoa rg) + somaIdades (rg - 1)

mediaIdade :: Int -> Int
mediaIdade x = (somaIdades x)`div`x

-- C)
sexo :: ([Char], Int, Char) -> Char
sexo (_,_,s) = s

sexoMasculino :: Int -> Int
sexoMasculino 0 = 0
sexoMasculino rg
    | sexo (pessoa rg) == 'm' = 1 + sexoMasculino (rg - 1)
    | otherwise = sexoMasculino (rg - 1)

-- D)
maiorIdade :: Int -> Int -> Int
maiorIdade maior 0 = maior
maiorIdade maior rg 
    | idade (pessoa rg) > idade (pessoa maior) = maiorIdade rg (rg - 1)
    | otherwise = maiorIdade maior (rg - 1)

maiorIdadeTotal :: Int -> Int
maiorIdadeTotal x = maiorIdade x x 

-- 3)
f3 :: Char -> (Char, Char, Int)
f3 x 
    | isUpper x = (x, toLower x, ord x)
    | otherwise = (x, toUpper x, ord x)

-- 4)
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

f4 :: (Int, Int, Int, Int) -> [Int]
f4 (a,b,c,d) = qSort (a:b:c:[d])

-- 5) ???

-- 6) ???

-- 7)
triangulo :: Int -> Int -> Int -> (String, Int)
triangulo x y z
    | (x == y) && (y == z) = ("Equilatero", (x+y+z))
    | (x == y) || (x == z) || (y == z) = ("Isoceles", (x+y+z))
    | (x <= 0) || (y <= 0) || (z <= 0) = ("Nao e triangulo", 0)
    | otherwise = ("Escaleno", (x+y+z))

-- 8)
type DadosBase = (Int,String,String,Char)
base::Int->DadosBase
base x
    |x == 0 = (1793, "Pedro Paulo" ,"MESTRE", 'M')
    |x == 1 = (1797, "Joana Silva Alencar","MESTRE", 'M')
    |x == 2 = (1534, "João De Medeiros" ,"DOUTOR", 'F')
    |x == 3 = (1267, "Cláudio César de Sá","DOUTOR", 'M')
    |x == 4 = (1737, "Paula de Medeiros" ,"MESTRE", 'F')
    |x == 5 = (1888, "Rita de Matos","MESTRE", 'F')
    |x == 10 = (0, "", "", '0')

titulo :: (Int, String, String, Char) -> String
titulo (_,_,t,_) = t

sexo2 :: (Int, String, String, Char) -> Char
sexo2 (_,_,_,s) = s

nome2 :: (Int, String, String, Char) -> String
nome2 (_,n,_,_) = n

registro :: (Int, String, String, Char) -> Int
registro (r,_,_,_) = r

-- A)
doutores :: Int -> Int
doutores (-1) = 0
doutores x
    | titulo (base x) == "DOUTOR" = 1 + doutores (x - 1)
    | otherwise = doutores (x - 1)

-- B)
mulheres :: Int -> Int
mulheres (-1) = 0
mulheres x
    | sexo2 (base x) == 'F' = 1 + mulheres (x - 1)
    | otherwise = mulheres (x - 1)

-- C)
mestres :: Int -> Int
mestres (-1) = 0
mestres x 
    | sexo2 (base x) == 'M' && titulo (base x) == "MESTRE" = 1 + mestres (x - 1)
    | otherwise = mestres (x - 1)

-- D)
antigo :: Int -> Int -> Int
antigo maisAntigo (-1) = maisAntigo
antigo maisAntigo x
    | registro (base x) < registro (base maisAntigo) = antigo x (x - 1)
    | otherwise = antigo maisAntigo (x - 1)

nomeAntigo :: Int -> String
nomeAntigo x = nome2 (base (antigo x x))
