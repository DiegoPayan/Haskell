
import NumeroRandom

import Data.List 
import Data.Ord (comparing)

tabla = [1,3,2,3,5,6,7,8]
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

jugadorComienzaInicio :: [[Int]]
jugadorComienzaInicio 
    | [5,5] `elem` jugador1 =[[1],[5,5]]
    | [5,5] `elem` jugador2  =[[2],[5,5]]
    | [0,5] `elem` jugador1 =[[1],[0,5]]
    | [0,5] `elem` jugador2  = [[2],[0,5]]
    | [2,3] `elem` jugador1 = [[1],[2,3]]
    | [2,3] `elem` jugador2  = [[2],[2,3]]
    | [6,4] `elem` jugador1 = [[1],[6,4]]
    | [6,4] `elem` jugador2  = [[2],[6,4]]
    | [1,4] `elem` jugador1 = [[1],[1,4]]
    | [1,4] `elem` jugador2  = [[2],[1,4]]
    | otherwise =[[0],[0,0]]

fichaPone ::[[Int]] -> Int -> [Int] -> [[Int]] 
fichaPone listaJugador turno aBorrar
    | length listaJugador > 0 = listaJugador \\ [aBorrar]
    | otherwise = [[0,0]]
   
concatenaFichaConLista :: Int -> [[Int]] ->[Int] ->Int ->[[Int]]
concatenaFichaConLista tipo xs x direccion
    | x == [5,5] && tipo == 1 = xs ++ [x] --1 =hori<ontal
    | x == [5,5] && tipo == 0 = xs ++ [x] --0 = vertical
    | tipo == 1 && direccion == 1 = [x] ++ xs --direccion = 1 =derecha
    | tipo == 1 && direccion == 0 = xs ++ [x] --direccion = 0 =izquierda
    |otherwise = [[]]

tomaCabecera :: [[Int]] -> [Int]
tomaCabecera xs = head xs

tomaUltimo :: [[Int]] -> [Int]
tomaUltimo xs = last xs

verificaQuePone :: [Int]->[Int]->[[Int]] ->[[Int]]->([Int],[[Int]],[[Int]])
verificaQuePone cabecera ultimo jugadorAct fichasRestantes 
    | length (filter (elem (head cabecera)) jugadorAct) > 0 && head (filter (elem (head cabecera)) jugadorAct) !!0 == head cabecera= (head (filter (elem (head cabecera)) jugadorAct) ++ [1,1,1], fichasRestantes ,jugadorAct)--1=reversa 1=horizontal 1=derecha 0=izquiera
    | length (filter (elem (head cabecera)) jugadorAct) > 0 && head (filter (elem (head cabecera)) jugadorAct) !!1 == head cabecera=( head (filter (elem (head cabecera)) jugadorAct) ++ [0,1,1],fichasRestantes,jugadorAct)--1=reversa 1=horizontal 1=derecha 0=izquiera
    | length (filter (elem (last ultimo)) jugadorAct ) > 0 && head (filter (elem (last ultimo)) jugadorAct)!!1 == last ultimo = (head (filter (elem (last ultimo)) jugadorAct) ++ [1,1,0],fichasRestantes ,jugadorAct)--0=dejalacomoesta 
    | length (filter (elem (last ultimo)) jugadorAct ) > 0 && head (filter (elem (last ultimo)) jugadorAct)!!0 == last ultimo  =( head (filter (elem (last ultimo)) jugadorAct) ++ [0,1,0], fichasRestantes,jugadorAct)--0=dejalacomoesta 
    | length fichasRestantes == 0 = ([0,0,0,0,0], fichasRestantes,jugadorAct)
    | otherwise = comeFichas cabecera ultimo jugadorAct fichasRestantes 

comeFichas  :: [Int] -> [Int] -> [[Int]] -> [[Int]] -> ([Int], [[Int]],[[Int]])
comeFichas cabecera ultimo jugadorAct fichasRestantes = verificaQuePone cabecera ultimo fichasNuevasJugador (drop 1 fichasRestantes)
    where fichasNuevasJugador = jugadorAct ++ take 1 fichasRestantes
   
           

acomodadorFichas:: Int -> [Int] -> [Int]
acomodadorFichas sino ficha
    | sino == 1 = reverse ficha
    | otherwise = ficha

sacaTurno:: Int -> Int
sacaTurno x
    | x == 1 = 2
    | otherwise = 1

main = do

    print "FICHAS JUGADOR 1:"
    print jugador1
    print "_______________________________"
    print "FICHAS JUGADOR 2:"
    print jugador2
    print "_______________________________"
    print "FICHAS RESTANTES"
    print fichasRestantes
    print "_______________________________"
    let quienIniciaTodo = jugadorComienzaInicio
    let quienInicia = quienIniciaTodo!!0
    let queTrae = quienIniciaTodo!!1
    print "TURNO JUGADOR: "
    print quienInicia
    print "TRAE LA CARTA:"
    print queTrae
    print "_______________________________"
    if quienInicia!!0 == 1
        then do
            let quePone = fichaPone jugador1 1 queTrae
            let listaJugadorN = quePone 
            let listaHorizontal = concatenaFichaConLista 1 [] queTrae 1 
            let listaVertical = concatenaFichaConLista 0 [] queTrae 1
            print "INICIO LISTAS:"
            print "Lista Horizontal:"
            print listaHorizontal
            print "Lista Vertical"
            print listaVertical
            print listaJugadorN
            siguienteTurno listaHorizontal listaVertical listaJugadorN 2 jugador2 0 0 fichasRestantes
        else do
            let quePone = fichaPone jugador2 2 queTrae
            let listaJugadorN = quePone 
            let listaHorizontal = concatenaFichaConLista 1 [] queTrae 1
            let listaVertical = concatenaFichaConLista 0 [] queTrae 1
            print "INICIO LISTAS:"
            print "Lista Horizontal:"
            print listaHorizontal
            print "Lista Vertical"
            print listaVertical
            print listaJugadorN
            siguienteTurno listaHorizontal listaVertical listaJugadorN 1 jugador1 0 0 fichasRestantes
    

           
siguienteTurno listaHorizontal listaVertical listaJugadorN turnoActual listaJugadorSig puntosJugador1 puntosJugador2 fichasRestantes= do
    print "SIGUIENTE TURNO"
    print turnoActual
    print listaJugadorSig
    let sacaTurnoNuevo = sacaTurno turnoActual
    print "_______________________________"
    let cabecera = tomaCabecera listaHorizontal
    let ultimo = tomaUltimo listaHorizontal
    let (fichaQuePondra, fichasRestantes1,fichasNuevasJugador) = verificaQuePone cabecera ultimo listaJugadorSig fichasRestantes
    let fichita = (take 2 fichaQuePondra)

    print "ff"
    print fichasRestantes1--
    if fichita == [0,0] && length fichasRestantes1 == 0 
        then do
            print "_______________________________"
            print "Acabo Juego"
            print "FICHAS JUGADOR :"
            print turnoActual
            print listaJugadorSig
            print "_______________________________"
            print "FICHAS JUGADOR :"
            print sacaTurnoNuevo
            print listaJugadorN
            print "_______________________________"
                else do 
            let listaJugadorNueva = fichaPone fichasNuevasJugador 1 fichita --
            let reverseo = (fichaQuePondra !! 2)
            let verticalUhorizontal = (fichaQuePondra!!3)
            let aQueDireccion = (fichaQuePondra!!4)
            let acomodaFicha = acomodadorFichas reverseo fichita
            print "PONDRA LA FICHA"
            print fichita
            print acomodaFicha
            print "_______________________________"
            let listaNuevaHorizontal = concatenaFichaConLista 1 listaHorizontal acomodaFicha aQueDireccion --
            print listaNuevaHorizontal    
            -- print verticalUhorizontal
            siguienteTurno listaNuevaHorizontal listaVertical listaJugadorNueva  sacaTurnoNuevo  listaJugadorN  0 0 fichasRestantes1
        -- print listaHorizontal
        -- print listaVertical
        -- print turnoActual
        -- print listaJugadorN
        -- print listaJugadorSig
