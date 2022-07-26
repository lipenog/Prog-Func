allDE :: Int -> Int -> Int -> Bool
allDE m n p =  (m/=n) && (n/=p)

-- O que está errado nessa definição?
-- m pode ser diferente de p 

allD :: Int -> Int -> Int -> Bool
allD m n p =  (m/=n) && (n/=p) && (m/=p)