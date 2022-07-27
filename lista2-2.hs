pessoa :: Int -> ([Char], Int, Char)
pessoa rg 
    | rg == 1 = ("Joao Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza", 51, 'm')
    | rg == 3 = ("Cleide", 22, 'f')
    | rg == 4 = ("Julia", 33, 'f')

-- A)
idadeI :: ([Char], Int, Char) -> Int
idadeI (_,i,_) = i

idade :: Int -> Int
idade x = idadeI (pessoa x)

nomeI :: ([Char], Int, Char) -> [Char]
nomeI (n,_,_) = n

nome :: Int -> [Char]
nome x = nomeI (pessoa x)

sexoI :: ([Char], Int, Char) -> Char
sexoI (_,_,s) = s

sexo :: Int -> Char
sexo x = sexoI (pessoa x)

menorIdade :: Int -> Int -> Int
menorIdade menorI 0 = menorI
menorIdade menorI rg
    | idade rg < idade menorI = menorIdade rg (rg - 1)
    | otherwise = menorIdade menorI (rg - 1)

menorIdadeP :: Int -> [Char]
menorIdadeP x = nome (menorIdade x x)

-- B)

sIdade :: Int -> Int
sIdade 0 = 0
sIdade x = idade x + sIdade (x - 1)

mediaIdade :: Int -> Int
mediaIdade x = (sIdade x) `div` x

-- C)

sexMasc :: Int -> Int
sexMasc 0 = 0
sexMasc x
    | sexo x == 'm' = 1 + sexMasc (x - 1)
    | otherwise = sexMasc (x - 1)

-- D)

maiorRG :: Int -> Int -> Int
maiorRG mRG 0 = mRG
maiorRG mRG rgs
    | idade rgs > idade mRG = maiorRG rgs (rgs - 1)
    | otherwise = maiorRG mRG (rgs - 1)

maiorRGP :: Int -> Int
maiorRGP x = maiorRG x x