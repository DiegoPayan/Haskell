import System.Random

fichas :: [[Integer]]
fichas = [[x,y] | x<-[0..6] , y<-[x..6] ]

fichaAleatoria :: [a] -> IO a
fichaAleatoria fichas = do
    i <- randomRIO (0, 27)
    return $ fichas !! i

jugador1 :: [Integer]
jugador1 = []

jugador2 :: [Integer]
jugador2 = []

main = do
        putStrLn "Inicia reparto de fichas"
        let fichasRepartidas = reparteFichas jugador1 jugador2
        putStrLn "Fin"
         -- putStrLn . show =<< rand

reparteFichas :: [Integer] -> [Integer] -> [[Integer]]
reparteFichas a b = [a]