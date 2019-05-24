import NumeroRandom

fichas :: [[Integer]]
fichas = [ [x,y] | x<-[0..6] , y<-[x..6] ]

random1 :: [Int]
random1 = [a,b,c,d,e,f,g]

jugador1 :: [Int]
jugador1 = []

random2 :: [Int]
random2 = [h,i,j,k,l,m,n]

jugador2 :: [Int]
jugador2 = []

main = do
        putStrLn "Inicia reparto de fichas"
        let fichasUno = reparteFichas random1
        let fichasDos = reparteFichas random2
        print fichasUno
        print fichasDos
        putStrLn "Fin"
         -- putStrLn . show =<< rand

reparteFichas rand = [fichas!!xs |xs <- rand]
