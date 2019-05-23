lista = [ x+y | x <- [1..20], y <- [1..100], x < 10, even y ]

logitud lista = sum[1 | x <- lista]