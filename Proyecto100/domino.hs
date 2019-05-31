
import NumeroRandom

import Data.List (sortBy)
import Data.Ord (comparing)

-- Lista de las 28 fichas del juego
fichas :: [[Int]]
fichas = [ [x,y] | x<-[0..6] , y<-[x..6] ]

random1 :: [Int]
random1 = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab]

fichasNumeradas :: [(Int, [Int])]
fichasNumeradas = [ (x,y) | (x,y) <- zip random1 fichas]

fichasDesordenadas :: [(Int, [Int])]
fichasDesordenadas = sortBy (comparing $ fst) fichasNumeradas

fichasDesordenadasSinIndice :: [[Int]]
fichasDesordenadasSinIndice = [ snd x | x<-fichasDesordenadas ]

jugador1 :: [[Int]]
jugador1 = take 7 fichasDesordenadasSinIndice

fichasDesordenadas1 :: [[Int]]
fichasDesordenadas1 = drop 7 fichasDesordenadasSinIndice

jugador2 :: [[Int]]
jugador2 = take 7 fichasDesordenadas1

fichasRestantes :: [[Int]]
fichasRestantes = drop 7 fichasDesordenadas1

comeCarta :: Int
comeCarta turno
    |turno == 1 = concatenaFichaCome 1 head fichasRestantes 
    |otherwise concatenaFichaCome 2 head fichasRestantes

jugadorComienzaInicio :: Int -> Int
jugadorComienza turno
jugadorComienzaInicio
    | [5,5] `elem` jugador1 = turnoJugador1
    | [5,5] `elem` jugador2  = turnoJugador2
    | otherwise = comeCarta turno

concatenaFichaCome  turno x |turno == 1 = jugador1 ++ [x] 
                            |otherwise jugador2 ++ [x] 

-- Función principal del programa
main = do
    putStrLn "Inicia reparto de fichas"
    jugadorComienza 1
    putStrLn "Fin"
    -- putStrLn . show =<< rand