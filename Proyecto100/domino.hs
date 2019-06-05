
import NumeroRandom

import Data.List 
import Data.Ord (comparing)

sumaPosibles = [5,10,15,20,25,30,35,40,45,50,55,60]

-- Lista de las 28 fichas del juego de domino
fichas :: [[Int]]
fichas = [ [x,y] | x<-[0..6] , y<-[x..6] ]

fichasNumeradas :: [(Int, [Int])]
fichasNumeradas = [ (x,y) | (x,y) <- zip listaRandomInt fichas]

fichasDesordenadas :: [(Int, [Int])]
fichasDesordenadas = sortBy (comparing $ fst) fichasNumeradas

fichasDesordenadasSinIndice :: [[Int]]
fichasDesordenadasSinIndice = [ snd x | x<-fichasDesordenadas ]

-- Fichas del jugador 1
jugador1 :: [[Int]]
jugador1 = take 7 fichasDesordenadasSinIndice

fichasDesordenadas1 :: [[Int]]
fichasDesordenadas1 = drop 7 fichasDesordenadasSinIndice

-- Fichas del jugador 1
jugador2 :: [[Int]]
jugador2 = take 7 fichasDesordenadas1

-- Fichas restantes después de repartir 7 fichas a cada jugador
fichasRestantes :: [[Int]]
fichasRestantes = drop 7 fichasDesordenadas1

multiplos5 :: [[Int]]
multiplos5 = [ [x,y] | x<-[0..6] , y<-[x..6], (x+y) `mod` 5 == 0, [x,y] /= [0,0] ]

jugador1ComienzaInicio = take 1 [ x | x <- multiplos5, x `elem` jugador1]
jugador2ComienzaInicio = take 1 [ x | x <- multiplos5, x `elem` jugador2]

jugador1Head = if jugador1ComienzaInicio /= [] then head jugador1ComienzaInicio else [0,0]
jugador2Head = if jugador2ComienzaInicio /= [] then head jugador2ComienzaInicio else [0,0]

jugadorComienzaInicio :: [[Int]]
jugadorComienzaInicio =
    if jugador1Head == [0,0] && jugador2Head == [0,0] then [[0],[0,0]]
    else if jugador1Head /= [0,0] then [[1],jugador1Head]
    else [[2],jugador2Head]

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
    | length (filter (elem (head cabecera)) jugadorAct) > 0 && head (filter (elem (head cabecera)) jugadorAct) !!0 == head cabecera = (head (filter (elem (head cabecera)) jugadorAct) ++ [1,1,1], fichasRestantes ,jugadorAct)--1=reversa 1=horizontal 1=derecha 0=izquiera
    | length (filter (elem (head cabecera)) jugadorAct) > 0 && head (filter (elem (head cabecera)) jugadorAct) !!1 == head cabecera = (head (filter (elem (head cabecera)) jugadorAct) ++ [0,1,1],fichasRestantes,jugadorAct)--1=reversa 1=horizontal 1=derecha 0=izquiera
    | length (filter (elem (last ultimo)) jugadorAct ) > 0 && head (filter (elem (last ultimo)) jugadorAct)!!1 == last ultimo = (head (filter (elem (last ultimo)) jugadorAct) ++ [1,1,0],fichasRestantes ,jugadorAct)--0=dejalacomoesta 
    | length (filter (elem (last ultimo)) jugadorAct ) > 0 && head (filter (elem (last ultimo)) jugadorAct)!!0 == last ultimo = (head (filter (elem (last ultimo)) jugadorAct) ++ [0,1,0], fichasRestantes,jugadorAct)--0=dejalacomoesta 
    | length fichasRestantes == 0 = ([100,100,0,0,0], fichasRestantes,jugadorAct) --100 sin cartas restantes
    | length jugadorAct == 0 = ([10,10,0,0,0], fichasRestantes,jugadorAct) --10 sin cartas
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

verificaSiGanaPuntos:: Int ->Int-> [[Int]]-> (Int, Int)
verificaSiGanaPuntos turno sumaPuntos lista
    | (head lista)!!0 == (head lista)!!1 && (last lista)!!0 == (last lista)!!1 && sumaMulas `elem` sumaPosibles =(turno,sumaMulas+sumaPuntos)
    | (head lista)!!0 == (head lista)!!1 && ((head lista)!!0 + (head lista)!!1+(last lista)!!1) `elem` sumaPosibles =(turno, (((head lista)!!0) +( (head lista)!!1) +((last lista)!!1)) +sumaPuntos)
    | (last lista)!!0 == (last lista)!!1 && ((head lista)!!0 + (last lista)!!0 + (last lista)!!1) `elem` sumaPosibles = (turno ,(((head lista)!!0 )+ ((last lista)!!0 )+ ((last lista)!!1))+sumaPuntos)
    | ((head lista)!!0 + (last lista)!!1) `elem` sumaPosibles && ((last lista)!!0  /= (last lista)!!1) = (turno, ((head lista)!!0 + (last lista)!!1 + sumaPuntos))
    | ((head lista)!!0 + (last lista)!!1) `elem` sumaPosibles && ((head lista)!!0  /= (head lista)!!1) = (turno, ((head lista)!!0 + (last lista)!!1 + sumaPuntos))
    | otherwise = (turno,0+sumaPuntos)
    where sumaMulas=(((head lista)!!0) +( (head lista)!!1) + ((last lista)!!0 )+((last lista)!!1))

sumaFichasRestantes:: [[Int]] -> Int
sumaFichasRestantes [] = 0
sumaFichasRestantes (x:xs) = sum x + sumaFichasRestantes xs

sumaAlMultiploCercano::Int ->Int
sumaAlMultiploCercano suma = 5 * (suma `div` 5)

quienGanador :: Int -> Int-> Int -> Int -> Int->(Int , Int)
quienGanador turnoActual puntosJugadorSig alMutiploCercano sacaTurnoNuevo puntosJugadorN 
    | puntosJugadorSig > puntosJugadorN = (turnoActual, (alMutiploCercano+puntosJugadorSig))
    | otherwise = (sacaTurnoNuevo, (alMutiploCercano + puntosJugadorN))

main = do
    putStrLn ("========================================================================================================")
    putStrLn ("|                                            INICIA EL JUEGO                                           |")
    putStrLn ("========================================================================================================")
    putStrLn ("|  FICHAS JUGADOR 1: " ++ show jugador1  ++ "                                       |")
    putStrLn ("========================================================================================================")
    putStrLn ("|  FICHAS JUGADOR 2: " ++ show jugador2  ++ "                                       |")
    putStrLn ("========================================================================================================")
    putStrLn ("|  FICHAS PARA COMER: " ++ show fichasRestantes ++ "  |")
    putStrLn ("========================================================================================================")
    putStrLn ""
    let quienIniciaTodo = jugadorComienzaInicio -- Nos dice que jugador inicia y con qué ficha
    let quienInicia = quienIniciaTodo!!0 -- Jugador que inicia
    let queTrae = quienIniciaTodo!!1 -- Ficha con que inicia
    putStrLn ("========================================================================================================")
    putStrLn ("|  Turno del jugador: " ++ show quienInicia ++ " - INICIA CON CARTA: " ++ show queTrae ++ "                                                    |")
    if quienInicia!!0 == 1
        then do 
            let quePone = fichaPone jugador1 1 queTrae
            let listaJugadorN = quePone 
            let listaHorizontal = concatenaFichaConLista 1 [] queTrae 1 
            let listaVertical = concatenaFichaConLista 0 [] queTrae 1
            putStrLn ("|  Inicio Listas Horizontal " ++ show listaHorizontal ++ "                                                                    |")
            putStrLn ("========================================================================================================")
            siguienteTurno listaHorizontal listaVertical listaJugadorN 2 jugador2 0 0 fichasRestantes
        else do
            let quePone = fichaPone jugador2 2 queTrae
            let listaJugadorN = quePone 
            let listaHorizontal = concatenaFichaConLista 1 [] queTrae 1
            let listaVertical = concatenaFichaConLista 0 [] queTrae 1
            putStrLn ("|  Inicio Listas Horizontal " ++ show listaHorizontal ++ "                                                                    |")
            putStrLn ("========================================================================================================")
            siguienteTurno listaHorizontal listaVertical listaJugadorN 1 jugador1 0 0 fichasRestantes

           
siguienteTurno listaHorizontal listaVertical listaJugadorN turnoActual listaJugadorSig puntosJugadorN puntosJugadorSig fichasRestantes = do
    putStrLn ""
    putStrLn ("========================================================================================================")
    putStrLn ("Turno Jugador " ++ show turnoActual)
    putStrLn ""
    putStrLn ("Fichas del jugador " ++ show turnoActual ++ ": " ++ show listaJugadorSig)
    let sacaTurnoNuevo = sacaTurno turnoActual
    let cabecera = tomaCabecera listaHorizontal
    let ultimo = tomaUltimo listaHorizontal
    let (fichaQuePondra, fichasRestantes1, fichasNuevasJugador) = verificaQuePone cabecera ultimo listaJugadorSig fichasRestantes
    let fichita = (take 2 fichaQuePondra)
    putStrLn ("Fichas en la reserva: " ++ show fichasRestantes1)
    if (fichita == [100,100] && length fichasRestantes1 == 0 ) || (fichita ==[10,10]|| length listaJugadorN == 0 )
        then do
            let fichasDelJugadorSig = sumaFichasRestantes listaJugadorSig
            let fichasDelJugadorN = sumaFichasRestantes listaJugadorN
            if puntosJugadorSig > puntosJugadorN then do
                let alMutiploCercano = sumaAlMultiploCercano fichasDelJugadorN
                let quienGana = turnoActual
                let quienPierde = sacaTurnoNuevo
                let puntosTotales = puntosJugadorSig + alMutiploCercano
                imprimeSalida quienGana listaJugadorSig puntosJugadorSig quienPierde listaJugadorN puntosJugadorN alMutiploCercano puntosTotales
                putStrLn ("")
            else do
                let alMutiploCercano = sumaAlMultiploCercano fichasDelJugadorSig
                let quienGana = sacaTurnoNuevo
                let quienPierde = turnoActual
                let puntosTotales = puntosJugadorN + alMutiploCercano
                imprimeSalida quienGana listaJugadorN puntosJugadorN quienPierde listaJugadorSig puntosJugadorSig alMutiploCercano puntosTotales
                putStrLn ("")
         else do 
            let listaJugadorNueva = fichaPone fichasNuevasJugador 1 fichita --
            let reverseo = (fichaQuePondra !! 2)
            let verticalUhorizontal = (fichaQuePondra!!3)
            let aQueDireccion = (fichaQuePondra!!4)
            let acomodaFicha = acomodadorFichas reverseo fichita
            putStrLn("Coloca la ficha " ++ show fichita ++ " en el orden " ++ show acomodaFicha  )
            let listaNuevaHorizontal = concatenaFichaConLista 1 listaHorizontal acomodaFicha aQueDireccion --
            print listaNuevaHorizontal   
            let (turno, puntosAct) =  verificaSiGanaPuntos turnoActual puntosJugadorSig listaNuevaHorizontal
            putStrLn ("")
            putStrLn ("Puntos: " ++ show puntosAct)
            putStrLn ("========================================================================================================")
            putStrLn ("")
            siguienteTurno listaNuevaHorizontal listaVertical listaJugadorNueva  sacaTurnoNuevo  listaJugadorN  puntosAct puntosJugadorN  fichasRestantes1


imprimeSalida quienGana listaGanador puntosGanador quienPierde listaPerdedor puntosPerdedor entregaFichas puntosTotales = do
    putStrLn ("========================================================================================================")
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn (" =========================================== Fin del juego =============================================")
    putStrLn ("|")
    putStrLn ("| Fichas del jugador " ++ show quienGana ++ ": " ++ show listaGanador  ++ " - Puntos acumulados: " ++ show puntosGanador ++ " puntos")
    putStrLn ("|")
    putStrLn ("|=======================================================================================================")
    putStrLn ("|")
    putStrLn ("| Fichas del jugador " ++ show quienPierde ++ ": " ++ show listaPerdedor  ++ "- Puntos acumulados: " ++ show puntosPerdedor ++ " puntos")
    putStrLn ("| Jugador " ++ show quienPierde ++ " entrega : " ++ show entregaFichas  ++ " puntos ")
    putStrLn ("|")
    putStrLn ("|=======================================================================================================")
    putStrLn ("|")
    putStrLn ("| El ganador es: " ++ "Jugador "++show quienGana ++ " con " ++ show puntosTotales ++ " puntos")
    putStrLn ("|")
    putStrLn (" =======================================================================================================")
    putStrLn ("")