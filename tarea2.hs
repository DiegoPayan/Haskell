
-- 1
areaRectangulos :: Int -> Int -> Int -> Int -> Int
areaRectangulos a b c d
    | a*b > c*d = a*b
    | otherwise = c*d

-- 2
siembra :: Int -> Bool
siembra n
    | n >= 11 || n == 1 = True
    | otherwise = False

-- 3
edad :: Int -> String
edad n
    | n > 0 && n <= 11 = "niño"
    | n > 11 && n <= 18 = "joven"
    | n > 18 && n <= 50 = "adulto"
    | n > 50 = "adultoMayor"

-- 4
auto :: Integer -> Integer -> Bool
auto a b
    | a - b >= 10 = True
    | otherwise = False

-- 5
alumno :: Int -> String
alumno n
    | n >= 95 && n <= 100 = "Sobresaliente"
    | n >= 90 && n < 95 = "Altamente Competente"
    | n >= 80 && n < 90 = "Competente"
    | n >= 70 && n < 80 = "Suficiente"
    | otherwise = "No competente"