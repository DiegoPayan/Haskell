
-- lista = [ Lo que vamos a mostrar | x <- [lista que filtramos], condiciones]

lista = [ x | x <- [1..20], x mod 2 == 1]

lista = [ x*10 | x <- [1..20], x mod 2 == 0]

cuentaCifras lista = [ if x < 10 then "una cifra" else "dos cifras" | x <- lista, odd x]