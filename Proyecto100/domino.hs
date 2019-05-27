
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

jugadorComienza :: Int
jugadorComienza
    | [5,5] `elem` jugador1 = 1
    | [5,5] `elem` jugador2 = 2
    | otherwise = 1

-- Función principal del programa
main = do
    putStrLn "Inicia reparto de fichas"
    putStrLn "Fin"
    -- putStrLn . show =<< rand


reparteFichas :: Int -> [[Int]] -> [[Int]]
reparteFichas i rand = [  fichas!!xs  | xs <- rand!!i ]

func i xs =
    if i < 7
        then do
            let ficha = reparteFichas i xs
            let fichaRes = concat ficha
            let lista = sacaFicha fichaRes fichas
            func (i+1) lista
        else
            i

sacaFicha :: [Int] -> [[Int]] -> [[Int]]
sacaFicha _ [] = []
sacaFicha x (y:ys)  | x ==  y = sacaFicha x ys
                    | otherwise = y : sacaFicha x ys

--Hacer un metodo toma y elimina, toma y elimina

{-
where removeItem x (y:ys)
                | y == [] = []
                | x == y = removeItem x ys
                | otherwise = y : removeItem x ys
-}
