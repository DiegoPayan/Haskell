import System.Random

fichas :: [[Integer]]
fichas = [[x,y] | x<-[0..6] , y<-[x..6] , x + y > 0]

newRand = randomRIO (0, 16 :: Int)

randomList :: Integer -> IO([Integer])
randomList 0 = return []
randomList n = do
    r <- randomRIO(1,16)
    rs <- randomList(n-1)
    return (r:rs)

main = do
         putStrLn "Inicia reparto de fichas"
         let muestra = fichas
         putStrLn ("Muestra:" ++ show muestra)
         let rand = randomList 10
         putStrLn . show =<< rand

-- let lista = randomRIO (0, 16 :: Int)
-- putStrLn . show =<< lista
-- let elem = muestra !! 3
-- putStrLn (" " ++ show elem)