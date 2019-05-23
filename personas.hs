-- base de datos
personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
			("Picasso","Pintura",1881,1973),
			("Beethoven","Musica",1770,1823),
			("Poincare","Ciencia",1854,1912),
			("Quevedo","Literatura",1580,1654),
			("Goya","Pintura",1746,1828),
			("Einstein","Ciencia",1879,1955),
			("Mozart","Musica",1756,1791),
			("Botticelli","Pintura",1445,1510),
			("Borromini","Arquitectura",1599,1667),
			("Bach","Musica",1685,1750)]

-- funci�n que proporciona la lista de la nombres de 
-- personas de la BD

nombres :: [(String,String,Int,Int)] -> [String]
nombres xs = [ x | (x,_,_,_) <- xs]

-- funci�n que retorna los nombres de las personas que son m�sicos
musicos :: [(String,String,Int,Int)] -> [String]
musicos xs = [ x | (x,y,_,_) <- xs, y == "Musica"]


-- funci�n que define los nombres de las personas cuya actividad es m
actividadM :: [(String,String,Int,Int)] -> [String]
actividadM xs = [ x | (x,y,_,_) <- xs, take 1 y == "M"]


-- Definir la funci�n vivas tal que (vivas bd a) es la lista de 
-- los nombres de las personas de la base de datos bd que 
-- estaban vivas en el a�o a

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas xs n = [ x | (x,_,i,f) <- xs, i < n, f > n]
