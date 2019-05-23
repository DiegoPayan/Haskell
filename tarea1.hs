-- Definir una función que nos diga qué meses corresponden  a cada estación
-- ejemplo: Primavera == Marzo, Abril, Mayo
meses :: String -> String
meses estacion
    | estacion == "Primavera"  = "Enero,Febrero,Marzo"
    | estacion == "Verano"  = "Abril,Mayo,Junio,Julio"
    | estacion == "Otoño"  = "Agosto,Septiembre,Octubre"
    | estacion == "Invierno"  = "Noviembre,Diciembre"
    | otherwise = "Estacion no reconocida"

--Definir la función mcd, tal que (mcd a b) es el máximo común divisor de a y b calculado mediante el algoritmo de Euclides. Por ejemplo, mcd 30 45 == 15
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- Calcula el factorial de un numero usando: a) guardas y b) Patrones
fact1 :: Integer ->  Integer
fact1 n
    | n > 0 = n * fact1(n-1)
    | otherwise = 1

fact2 :: Int -> Int
fact2 0 = 1
fact2 n = n * fact2(n-1)

-- Decir hacia donde miras si giras 90 grados
type Direccion = String

girar90 :: Direccion -> Direccion
girar90 "derecha" = "atras"
girar90 "atras" = "izquierda"
girar90 "izquierda" = "frente"
girar90 "frente" = "derecha"
girar90 _ = "Lado equivocado!"

-- Decir que color sigue en el semaforo a partir del color actual
type Color = String
semaforo :: Color -> Color
semaforo color
    | color == "rojo" = "amarillo"
    | color == "amarillo" = "verde"
    | color == "verde" = "rojo"
    | otherwise = "color incorrecto"

-- Dice si el parametro b es multiplo del parametro a
multiploDe :: Integer -> Integer -> Bool 
multiploDe a b
    | b `mod` a == 0 = True
    | otherwise = False

-- Retorna las raíces de una funcion en base a sus coeficientes
raíces :: Float -> Float -> Float -> (Float, Float)
raíces a b c = ((-b+num)/2.0*a,(-b-num)/2.0*a) where num = sqrt(b*b-4.0*a*c)

-- Funcion que reciba un numero y retorne cero
cero :: Integer -> Integer
cero _ = 0

-- Recibe la edad de una persona y le dice si puede votar
votar :: Int -> Bool
votar n
    | n >= 18 = True
    | otherwise = False

-- Dice si se presenta examen, estan excentos solo los 100 
-- excenta :: (Eq a, Num a) => a -> [Char]

