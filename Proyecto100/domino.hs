import NumeroRandom

-- Lista de las 28 fichas del juego
fichas :: [[Integer]]
fichas = [ [x,y] | x<-[0..6] , y<-[x..6] ]

-- Lista de randoms para seleccionar fichas para el jugador 1
random1 :: [Int]
random1 = [a,b,c,d,e,f,g]

-- Lista de randoms para seleccionar fichas para el jugador 2
random2 :: [Int]
random2 = [h,i,j,k,l,m,n]

-- Fichas del jugador 1
jugador1 :: [Int]
jugador1 = []

-- Fichas del jugador 2
jugador2 :: [Int]
jugador2 = []

-- Función principal del programa
main = do
        putStrLn "Inicia reparto de fichas"
        let fichasUno = reparteFichas random1
        let fichasDos = reparteFichas random2
        print fichasUno
        print fichasDos
        putStrLn "Fin"
         -- putStrLn . show =<< rand

reparteFichas rand = [ fichas!!xs | xs <- rand ]
        
--Hacer un metodo toma y elimina, toma y elimina

{-
where removeItem x (y:ys)
                | y == [] = []
                | x == y = removeItem x ys
                | otherwise = y : removeItem x ys
-}
