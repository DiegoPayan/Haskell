import System.Random

main = do
    putStr . show =<< randomRIO (0, 16 :: Int)
    putStr ", "
    print         =<< randomRIO (0, 16 :: Int)
    let lista = randomRIO (0, 16 :: Int)
    putStr . show =<< lista
   
deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (x:xs) = x : deleteDuplicate (filter (/= x) xs)