module NumeroRandom where

    import System.Random
    import System.IO.Unsafe

    a :: Int
    a = unsafePerformIO (getStdRandom (randomR (0, 27)))
    b :: Int
    b = unsafePerformIO (getStdRandom (randomR (0, 26)))
    c :: Int
    c = unsafePerformIO (getStdRandom (randomR (0, 25)))
    d :: Int
    d = unsafePerformIO (getStdRandom (randomR (0, 24)))
    e :: Int
    e = unsafePerformIO (getStdRandom (randomR (0, 23)))
    f :: Int
    f = unsafePerformIO (getStdRandom (randomR (0, 22)))
    g :: Int
    g = unsafePerformIO (getStdRandom (randomR (0, 21)))

    h :: Int
    h = unsafePerformIO (getStdRandom (randomR (0, 20)))
    i :: Int
    i = unsafePerformIO (getStdRandom (randomR (0, 19)))
    j :: Int
    j = unsafePerformIO (getStdRandom (randomR (0, 18)))
    k :: Int
    k = unsafePerformIO (getStdRandom (randomR (0, 17)))
    l :: Int
    l = unsafePerformIO (getStdRandom (randomR (0, 16)))
    m :: Int
    m = unsafePerformIO (getStdRandom (randomR (0, 15)))
    n :: Int
    n = unsafePerformIO (getStdRandom (randomR (0, 14)))

