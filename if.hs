
divisible x y = if ( x `mod` y ) == 0
    then "Son divisibles"
    else "No son divisibles"

esMayor x y = if ( x > y )
    then "Es mayor"
    else "No es mayor"

sumaDiezAMayoresQueVeinte x = if x > 20
    then x + 10
    else x