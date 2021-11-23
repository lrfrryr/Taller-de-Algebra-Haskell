--Ejercicio 1) a)
sumaDeDivisoresPropios :: Int -> Int
sumaDeDivisoresPropios n | n == 0 = 0
                         | otherwise = sumaDeDivisoresDesde n n - n 

--Función auxiliar para Ejercicio 1)a)
sumaDeDivisoresDesde :: Int -> Int -> Int
sumaDeDivisoresDesde n m | m == 0 = 0
                         | mod n m == 0 = m + sumaDeDivisoresDesde n (m-1)
                         | otherwise = sumaDeDivisoresDesde n (m-1)

--Ejercicio 1) b) 
esPerfecto :: Int -> Bool
esPerfecto n | n == 0 = False 
             | otherwise = sumaDeDivisoresPropios n == n 

--Ejercicio 2) a) Devuelve una lista con los primeros k elementos de la lista alicuota de n 
listaAlicuotaDeNDeLargo :: Int -> Int -> [Int]
listaAlicuotaDeNDeLargo 0 _ = []
listaAlicuotaDeNDeLargo k n = n:listaAlicuotaDeNDeLargo (k-1) (sumaDeDivisoresPropios n)

--Ejercicio 2) b) Determina si una lista dada es un club
sonSociables :: [Int] -> Bool
sonSociables [] = False
sonSociables (x:xs) | distintos (x:xs) && ultimoIgualPrimero (x:xs) && chequeoSumaSiguiente (x:xs) = True
                    | otherwise = False 

--Auxiliar Ejercicio 2)b), chequea que la suma de divisores propios del último término coincida con el valor del primer término
ultimoIgualPrimero :: [Int] -> Bool
ultimoIgualPrimero [] = False
ultimoIgualPrimero (x:xs) | xs == [] = ultimoIgualPrimero (x:[x])
                          | sumaDeDivisoresPropios (ultimo xs) == x = True
                          | otherwise = False  

--Auxiliar Ejercicio 2)b), chequea que la suma de divisores propios de un elemento de la lista sea igual al número siguiente de la lista
chequeoSumaSiguiente :: [Int] -> Bool
chequeoSumaSiguiente [] = False
chequeoSumaSiguiente (x:xs) | xs == [] = True
                            | sumaDeDivisoresPropios x == head (xs) = chequeoSumaSiguiente xs
                            | otherwise = False 

--Auxiliar Ejercicio 2)b), chequea que todos los elementos de la lista sean distintos
distintos :: Eq a => [a] -> Bool
distintos [] = True
distintos [_] = True
distintos (x:xs) | x `elem` xs = False --elem chequea si el primer elemento se encuentra dentro de la lista xs. Si pertenece, ya tengo al menos dos elementos iguales, entonces distintos es falso.
                 | otherwise = distintos xs --si el primer elemento no pertenece, arranco de nuevo a chequear con los elementos de la cola

--Otra forma de implementar una función que haga lo mismo que elem es usar la función pertenece:
--pertenece :: Int -> [Int] -> Bool
--pertenece x l | l == [] = False
--              | otherwise = x == head (l) || pertenece x (tail l)
--En este caso usé elem para no tener tantas funciones auxiliares y no tener problemas con los tipos

--Auxiliar de Ejercicio 2), me devuelve el último término de una lista
ultimo :: [a] -> a
ultimo []     = error "Lista vacía"
ultimo [x]    = x
ultimo (_:xs) = ultimo xs

--Ejercicio 3)a)
minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue _ 0 = []
minimosDeKClubesMenoresQue k c | sonSociables (l) = agregar (minimo ((l))) (minimosDeKClubesMenoresQue k (c-1))
                               | otherwise = minimosDeKClubesMenoresQue k (c-1)
                               where l = listaAlicuotaDeNDeLargo k c

--Auxiliares ejercicio 3)a):

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

--algo clave de la función auxiliar agregar para el ejercicio 3)a) es que chequea antes de agregar si el elemento ya está en la lista (si no, aparecerían elementos repetidos)
agregar :: Int -> [Int] -> [Int]
agregar x c | pertenece x c = c
            | otherwise = x:c

mini :: Int -> Int -> Int
mini a b
    | a > b  = b
    | a < b  = a
    | a == b = a

minimo :: [Int] -> Int
minimo []       = 0
minimo [x]      = x
minimo (x:xs)   = mini x (minimo xs)

--Ejercicio 3)b) devuelve una lista de todos los clubes con n miembros cuyos elementos son todos menores que k
listaDeNClubesConNrosMenoresQue :: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue _ 0 = [[]]
listaDeNClubesConNrosMenoresQue 0 _ = []
listaDeNClubesConNrosMenoresQue n k | sonSociables (j) == True && menoresQue (j) k == True = (listaDeNClubesConNrosMenoresQue n (k-1)) `unir` [j] --condiciones: que sean sociables y menores a k
                                    | otherwise = listaDeNClubesConNrosMenoresQue n (k-1)
                                    where j = listaAlicuotaDeNDeLargo n (k-1) -- acá uso k-1 en vez de k porque de otra forma me daría vacío


--Auxiliares del Ejercicio 3)b)

-- usé unir en vez de agregar del ejercicio 3)a) porque necesitaba una función que permitiera otros tipos (listas de listas de enteros)
unir :: [[Int]] -> [[Int]] -> [[Int]]
unir a b | a == [[]] || a == [] = b 
         | otherwise = (head a):(unir (tail a) b)

menoresQue :: [Int] -> Int -> Bool
menoresQue [] _ = True
menoresQue (x:xs) k | x < k && menoresQue (xs) k == True = True
                       | otherwise = False 

