import System.Random

fichas :: [[Integer]]
fichas = [ [x,y] | x<-[0..6] , y<-[x..6] ]

fichasRandomUno xs = do
    a <- randomRIO(0, 27) :: IO Int
    b <- randomRIO(0, 26) :: IO Int
    c <- randomRIO(0, 25) :: IO Int
    d <- randomRIO(0, 24) :: IO Int
    e <- randomRIO(0, 23) :: IO Int
    f <- randomRIO(0, 22) :: IO Int
    g <- randomRIO(0, 21) :: IO Int
    let numbers = xs ++ a:[ b, c, d, e, f, g]
    print numbers

fichasRandomDos xs = do
    a <- randomRIO(0, 20) :: IO Int
    b <- randomRIO(0, 19) :: IO Int
    c <- randomRIO(0, 18) :: IO Int
    d <- randomRIO(0, 17) :: IO Int
    e <- randomRIO(0, 16) :: IO Int
    f <- randomRIO(0, 15) :: IO Int
    g <- randomRIO(0, 14) :: IO Int
    let numbers = xs ++ a:[ b, c, d, e, f, g]
    print numbers

jugador1 :: [Int]
jugador1 = []

jugador2 :: [Int]
jugador2 = []

main = do
        putStrLn "Inicia reparto de fichas"
        let randomUno = fichasRandomUno jugador1
        let randomDos = fichasRandomDos jugador2
        --let fichasUno = reparteFichas randomUno
        --let fichasDos = reparteFichas randomDos
        putStrLn "Fin"
         -- putStrLn . show =<< rand

-- reparteFichas :: [Integer] -> Integer -> Integer
reparteFichas rand = [ xs |xs <- rand]
