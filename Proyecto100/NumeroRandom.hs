module NumeroRandom where

    import System.Random
    import System.IO.Unsafe
    
    listaRandom :: Int -> IO ([Int])
    listaRandom 0 = return []
    listaRandom n = do
        r  <- randomRIO(0,27) :: IO Int
        rs <- listaRandom (n-1)
        return (r:rs)
    
    listaRandomInt = unsafePerformIO $ (listaRandom 27)