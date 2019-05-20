import System.Random

fichas :: [[Integer]]
fichas = [[x,y] | x<-[0..6] , y<-[x..6] , x + y > 0]

newRand = randomIO :: IO Int

main = do
         putStrLn "Inicia reparto de fichas"
         let muestra = fichas
         putStrLn ("Muestra:" ++ show muestra)
         putStrLn ""
        