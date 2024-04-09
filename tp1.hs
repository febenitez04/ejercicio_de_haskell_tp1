import GHC.Float (int2Float)
{-titular:: Int
titular  = 149000

adjunto:: Int
adjunto = 116000

ayudante:: Int
ayudante = 66000-}

sueldoBase:: String -> Float
sueldoBase"titular" = 149000
sueldoBase "adjunto" = 116000
sueldoBase "ayudante"= 66000

cantDeHoras:: Int -> String -> Float
cantDeHoras horas cargo
    | horas == 10 && (horas <= 14) = 1 
    | horas > 14 && (horas <= 24) = 2 
    | horas > 24 && (horas <= 34) = 3 
    | horas > 34 && (horas <= 44) = 4 
    | horas > 44 && horas < 50 = 5 
    | otherwise = 0

antiguedad:: Int -> Int -> String -> Float
antiguedad anio cantHoras cargo
    | anio >= 3 && anio < 5 = 1.2 
    | anio >=5 && anio < 9 = 1.3 
    | anio >=10 && anio < 23 = 1.5 
    | anio >= 24 && anio > 24 = 2.2 
    | otherwise = 0

sueldoFinal:: Int -> String -> Int -> Float
sueldoFinal horas cargo anios =  cantDeHoras horas cargo * antiguedad anios horas cargo * sueldoBase cargo

