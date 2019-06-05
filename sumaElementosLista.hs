
lista = [ [x,y] | x <- [1..5], y <- [1..5]]

sumaFichasRestantes [] = 0
sumaFichasRestantes (x:xs) = sum x + sumaElementosLista xs