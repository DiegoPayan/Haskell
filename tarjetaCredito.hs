
digitosR :: Integer -> [Integer]
digitosR n
    | n < 10 = [n]
    | otherwise = (rem n 10) : digitosR (div n 10)

-- Forma 1
doblesPosImpar :: [Integer] -> [Integer]
doblesPosImpar [] = []
doblesPosImpar [x] = [x]
doblesPosImpar (x:y:xs) = x : y * 2 : doblesPosImpar xs

-- Forma 2
doblesPosImpar2 :: [Integer] -> [Integer]
doblesPosImpar2 (x:y:xs) = x : y * 2 : doblesPosImpar2 xs
doblesPosImpar2 xs = xs

--Forma 3 fx es una funcion local, se declaran con where
-- even para saber si es par , odd es la contrario
doblesPosImpar3 :: [Integer] -> [Integer]
doblesPosImpar3 xs = [ fx x y | (x,y) <- zip [0..] xs ]
    where fx n d | odd n = 2 * d
                 | otherwise = d

sumaDigitos :: [Integer] -> Integer
sumaDigitos [] = 0
sumaDigitos (x:xs) = sum (digitosR x) + sumaDigitos xs
