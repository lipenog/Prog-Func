base :: Int -> (Int, String, String, Char)
base x
    |x == 0 = (1793, "Pedro Paulo" ,"MESTRE", 'M')
    |x == 1 = (1797, "Joana Silva Alencar","MESTRE", 'M')
    |x == 2 = (1534, "João De Medeiros" ,"DOUTOR", 'F')
    |x == 3 = (1267, "Cláudio César de Sá","DOUTOR", 'F')
    |x == 4 = (1737, "Paula de Medeiros" ,"MESTRE", 'F')
    |x == 5 = (1888, "Rita de Matos","MESTRE", 'F')
    |x == 9 = (1698, "Tereza Cristina Andrade" ,"MESTRE", 'F')
    |x == 10 = (0, "", "", '0')
