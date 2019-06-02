module Main where
  import Lib
  import System.Random
  import Control.Monad
  import Data.List
  --  Stack ghci 
  
  
  domino = [[6,6],[6,5],[6,4],[6,3],[6,2],[6,1],[6,0],[5,5],[5,4],[5,3],[5,2],[5,1],[5,0],[4,4],[4,3],[4,2],[4,1],[4,0],[3,3],[3,2],[3,1],[3,0],[2,2],[2,1],[2,0],[1,1],[1,0],[0,0]]
  dominoPositions = [0..27]
  
  -- Obtiene 14 fichas de manera aleatoria para repartir entre los 2 jugadores
  getRandoms :: StdGen -> Int -> [Int] -> Bool -> Int -> [Int]
  getRandoms gen 14 items inList random = items
  
  getRandoms gen c items True random = do
    let (number, generator) = randomR(0,27) gen ::(Int,StdGen)
    getRandoms generator c items (isMember number items) number
  
  getRandoms gen c items False random = do
    let (number, generator) = randomR(0,27) gen ::(Int,StdGen)
    let newItems = random:items
    let member = isMember number newItems
    getRandoms generator (c+1) newItems member number
  
  -- Función que checa si un elemento está en la lista.
  isMember n [] = False
  isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs
  
  --  Funciónes que parten a la mitad la lista de 14 elementos 1 lista para cada jugador.
  tilesPlayer1 :: [Int] -> [Int]
  tilesPlayer1 e = [e!!a | a <- [0..6]]
  
  tilesPlayer2 :: [Int] -> [Int]
  tilesPlayer2 e = [e!!a | a <- [7..13]]
  
  --  Función que cambia las posiciones de los domino a las piezas reales.
  castIndexToTiles :: [Int] -> [[Int]]
  castIndexToTiles positions = map (\i -> domino!!i) positions
  
  -- Está función te hace comer tiles hasta que puedas poner algo ? PERSO SOLO AL INICIO DEL JUEGO
  castEatToMultiplyOfFive :: StdGen -> [[Int]] -> [[Int]] -> Int -> [[Int]]
  castEatToMultiplyOfFive _ _ player 1 = player
  castEatToMultiplyOfFive gen eatTiles playerTiles flag = do
    let (number, generator) = randomR(0,length eatTiles - 1) gen ::(Int,StdGen)
    let eatedTail = eatTiles!!number
    if head eatedTail `mod` 5 == 0 || eatedTail!!1 `mod` 5 == 0
      then do
        let nPTiles = eatedTail:playerTiles
        let updatedEatTiles = eatTiles \\ [eatedTail]
        castEatToMultiplyOfFive gen updatedEatTiles nPTiles 0
      else do
        let nPTiles = eatedTail:playerTiles
        let updatedEatTiles = eatTiles \\ [eatedTail]
        castEatToMultiplyOfFive gen updatedEatTiles nPTiles 1
  
  -- Toma la mejor decisión y me regresa donde ubicar la ficha
  getMax :: [([Int],Int)] -> ([Int],Int) -> [Int]
  getMax [] (x,xs)
    | xs == (-1) = []
    | otherwise = x
  getMax tuplas result = do
    let (_, sm) = head tuplas
    let (_, rsm) = result
    if sm > rsm then
      getMax (tail tuplas) (head tuplas)
      else
        getMax (tail tuplas) result 
  -- Retornar el multiplo más alto de 5 respecto a otro dato.
  -- Si no encuentra multiplos retorna alguna ficha que se pueda poner 
  -- Si no encuentra ninguna ficha para poner ni multiplos entonces regresa una lista vacía.
  -- Checador -> [tiles] -> 1 derecha 0 izquierda -> [Bigger] -> [[Int]]
  bestNumber :: Int -> [[Int]] -> Int -> [Int] -> [([Int],Int)]
  bestNumber number [] orientation bigger
    | head bigger == (-1) && bigger!!1 == (-1) = []
    | otherwise = [([head bigger, bigger!!1] ++ [orientation],bigger!!orientation + number)]
  bestNumber n t orientation big
    | (head t !! orientation + n) > (big!!1 + n) && (head t !! orientation + n) `mod` 5 == 0 = bestNumber n (tail t) orientation (head t)
    | (big!!orientation + n) `mod` 5 /= 0 && (head t !! orientation == n) = bestNumber n (tail t) orientation (head t)
    | otherwise = bestNumber n (tail t) orientation big
  
  -- Revisa el tablero y le devuelve al jugador la mejor opción
  -- Para poner en el tablero en el caso de que no encuentre
  -- Retorna un arreglo vacío y el jugador tiene que comer.¡'
  
  -- [Horizontal] -> [center] -> [Vertical] -> [tiles] -> Location
  bestWayToPut :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Bool -> Int -> ([[Int]],[[Int]],[[Int]])
  -- Cuando no se ha puesto nada en las horillas de la primera ficha.
  bestWayToPut [] center [] playerTiles _ _ = ([], [], playerTiles)
  -- bestResult = getMax (filter (not . null) (bestNumber 4 [[1,1],[2,4],[5,5]] 1 [-1,-1] ++ bestNumber 1 [[5,1],[10,2]] 0 [-1,-1])) ([],-1)
  -- bestWayToPut [[5,3],[4,5]] [[6,5]] [[3,5]] [[1,2],[3,4],[5,5],[1,1]] False 100
  bestWayToPut horizontal center vertical [] True location = (horizontal, center, vertical) -- Regresamos la lista vacía
  bestWayToPut horizontal _ vertical playerTiles True location
    | location == 1 = (horizontal, vertical, playerTiles) -- ABAJO
    | location == 2 = (horizontal, vertical, playerTiles) -- ARRIBA
    | location == 3 = (horizontal, vertical, playerTiles) -- DERECHA
    | otherwise = (horizontal, vertical, playerTiles) -- IZQUIERDA
  
  bestWayToPut horizontal center vertical playerTiles False _ = do
    let rigth = last horizontal !!1 -- Ultimo valor derecho
    let left = head (head horizontal) -- Ultimo valor izquierdo
    let top = head (head vertical) -- Ultimo arriba
    let bottom = last vertical !!1 -- Ultimo valor abajo
    let mayTop = filter (\i -> head i == top || i!!1 == top) playerTiles
    let mayBottom = filter (\i -> head i == bottom || i!!1 == bottom) playerTiles
    let mayLeft = filter (\i -> head i == left || i!!1 == left) playerTiles
    let mayRight = filter (\i -> head i == rigth || i!!1 == rigth) playerTiles
    if head (head center) == 5 && head center !!1 == 5 then -- Si el centro es la mula de 5
      if null mayLeft && null mayRight && null mayTop && null mayBottom then -- A comer
        bestWayToPut horizontal center vertical playerTiles True 100
        else
          bestWayToPut horizontal center vertical playerTiles True 100
    else -- Si el centro es cualquier otra cosa
      if null mayLeft && null mayRight then bestWayToPut horizontal center vertical playerTiles True 100 -- A comer
      else do
        -- let pairRightLeft = filter (\i -> (i!!1 + left) `mod` 5 == 0) mayRight
        -- let pairLeftRight = filter (\i -> (head i + rigth) `mod` 5 == 0) mayLeft
        let bestResult = getMax (filter (not . null) (bestNumber left mayRight 1 [-1,-1] ++ bestNumber rigth mayLeft 0 [-1,-1])) ([],-1)
        if null bestResult then bestWayToPut horizontal center vertical [] True 100
        else
          if bestResult!!2 == 0 then do -- Si 0 se ponen en la derecha si 1 se ponen en la izquierda
            let cleanResult = take 2 bestResult
            let newHorizontal = horizontal ++ [cleanResult]
            let newPTiles = playerTiles \\ [cleanResult]
            bestWayToPut newHorizontal center vertical newPTiles True 3
            else do
              let cleanResult = take 2 bestResult
              let newHorizontal = cleanResult : horizontal
              let newPTiles = playerTiles \\ [cleanResult]
              bestWayToPut newHorizontal center vertical newPTiles True 4
  -- !! El JUEGO !!!
  --     1            2           3            4            5              6            7        8
  -- [Horizontal] -> [Center] -> [Vertical] -> [tiles 1] -> [tiles 2] -> [EatTiles] -> [P1P] -> [P2P] -> Aleatorio -> Ronda -> Turno
  game :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [Int] -> [Int] -> StdGen -> Int -> Int -> ([[Int]],[[Int]],[[Int]],[[Int]],Int)
  
  
  -- -- * CUANDO YA NO HAY FICHAS PARA COMER
  -- game horizontal centro vertical tiles1 tiles2 [] p1p p2p gen rond playerTurn
  --   | True = (centro,tiles1,tiles2,[],playerTurn)
  --   | otherwise = game horizontal centro vertical tiles1 tiles2 [] p1p p2p gen rond playerTurn
  
  -- * CUANDO JUGADOR 1 NO TIENE MÁS FICHAS
  game horizontal centro vertical [] tiles2 eatTiles p1p p2p gen _ playerTurn = (centro,[[]],tiles2,eatTiles,playerTurn)
  
  -- * CUANDO JUGADOR 2 NO TIENE MÁS FICHAS
  game horizontal centro vertical tiles1 [] eatTiles p1p p2p gen _ playerTurn = (centro,tiles1,[[]],eatTiles,playerTurn)
  
  -- * CUANDO YA HAY LA PRIMERA FICHA EN EL JUEGO
  game horizontal centro vertical tiles1 tiles2 eatTiles p1p p2p gen 2 playerTurn = (centro,tiles1,tiles2,eatTiles,playerTurn)
  
  -- * EL INICIO DEL JUEGO
  game [] [] [] tiles1 tiles2 eatTiles p1p p2p gen 1 playerTurn = do
    let p1Entrada = filter (\i -> (head i + i!!1) == 10 && head i == i!!1) tiles1
    let p2Entrada = filter (\i -> (head i + i!!1) == 10 && head i == i!!1) tiles2
    if null p1Entrada && null p2Entrada -- Aquí es cuando nadie tiene la mula
      then do -- Si nadie tiene la mula de 5
        let multilply5pl1 = filter (\i -> (head i `mod` 5 == 0 ||  i!!1 `mod` 5 == 0) && i!!1 /= 0 && head i /= 0) tiles1
        let multilply5pl2 = filter (\i -> (head i `mod` 5 == 0 ||  i!!1 `mod` 5 == 0) && i!!1 /= 0 && head i /= 0) tiles2
        if null multilply5pl1 && null multilply5pl2 -- Checar si alguno tiene algun multiplo de 5
          then do -- Nadie tiene multiplo de 5
            let (whoStart, udgen) = randomR(0,1) gen::(Int,StdGen)
            if whoStart == 1 then do
              let playerTiles = castEatToMultiplyOfFive udgen eatTiles tiles2 0
              let updatedEatTiles = eatTiles \\ playerTiles
              game [] [] [] tiles1 playerTiles updatedEatTiles p1p p2p udgen 1 1 -- El player 2 empieza comiendo.
            else do
              let playerTiles = castEatToMultiplyOfFive udgen eatTiles tiles2 0
              let updatedEatTiles = eatTiles \\ playerTiles
              game [] [] [] playerTiles tiles2 updatedEatTiles p1p p2p udgen 1 2-- El player 1 empieza comiendo.
          else -- Aquí alguien ya se digno a tener un multiplo de 5
            if length multilply5pl1 > length multilply5pl2  -- Revisa quien fue y pone la ficha en el tablero
              then do
                let selectedTile = [head multilply5pl1]
                game selectedTile selectedTile selectedTile (tiles1 \\ selectedTile) tiles2 eatTiles p1p p2p gen 2 2
              else do
                let selectedTile = [head multilply5pl2]
                game selectedTile selectedTile selectedTile tiles1 (tiles2 \\ selectedTile) eatTiles p1p p2p gen 2 1
      else -- Revisa quien fue el que tiene la mula y actualiza sus fichas.
        if length p1Entrada > length p2Entrada
          then
            game p1Entrada p1Entrada p1Entrada (tiles1 \\ p1Entrada) tiles2 eatTiles p1p p2p gen 2 2
          else
            game p2Entrada p2Entrada p2Entrada tiles1 (tiles2 \\ p2Entrada) eatTiles p1p p2p gen 2 1
  
  main = do
    -- clearScreen
    g <- getStdGen
    let randomTiles = getRandoms g 0 [] True 0
    let tilesPlayerOne = castIndexToTiles (tilesPlayer1 randomTiles)
    let tilesPlayerTwo = castIndexToTiles (tilesPlayer2 randomTiles)
    let remainingPieces = castIndexToTiles (dominoPositions \\ randomTiles)
    let (list,tiles1,tiles2,eatTiles,turn) = game [] [] [] tilesPlayerOne tilesPlayerTwo remainingPieces [] [] g 1 (-1)
    print list
    print tiles1
    print tiles2
    print eatTiles
    print turn
    print ""