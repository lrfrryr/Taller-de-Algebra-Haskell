--Ejercicio 1: Función para Modelo Exponencial Discreto

med :: Float -> Float -> Int -> Float
med i0 b n | n == 0 = i0
           | otherwise = med i0 b (n-1) * (1+b) --factor común del llamado recursivo



--Ejercicio 2: Función para Modelo Logístico Discreto

mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b n | n == 0 = i0
             | otherwise = mld p i0 b (n-1) * (1 + b*(sanosAnt/p)) --factor común del llamado recursivo
             where
                sanosAnt = p-(mld p i0 b (n-1)) --a la población se le restan los infectados del día anterior
                                               --sanosAnt/p = proporción de sanos del día anterior respecto de la población total
                


--Ejercicio 3: Función para Modelo SIR Discreto

sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) b g n | n == 0 = (s0, i0, r0)
                       | otherwise = ((sanost s0 i0 b g n), (infect s0 i0 b g n), (recupt r0 s0 i0 b g n)) --tupla de funciones auxiliares recursivas de cada parámetro s, i, r

--FUNCIONES AUXILIARES EJERCICIO 3

--Función auxiliar de Ejercicio 3 del parámetro s (individuos sanos) a los t días
--Cantidad de individuos sanos se reduce de acuerdo a la tasa de infección (b), la cantidad de individuos infectados y la cantidad de individuos sanos del día anterior

sanost :: Float -> Float -> Float -> Float -> Int -> Float
sanost s0 i0 b g n | n == 0 = s0
                   | otherwise = sanost s0 i0 b g (n-1) * (1 - b*(infect s0 i0 b g (n-1))) --factor común del llamado recursivo. Llama a otra función (infect)

--Función auxiliar de Ejercicio 3 (y Ejercicio 4) del parámetro i (individuos infectados) a los t días
--Cantidad de individuos infectados se reduce proporcionalmente a la tasa de recuperación (g), y aumenta de acuerdo a la tasa de infección (b)

infect :: Float -> Float -> Float -> Float -> Int -> Float
infect s0 i0 b g n | n == 0 = i0
                   | otherwise = infect s0 i0 b g (n-1) * (1 + b*(sanost s0 i0 b g (n-1)) - g) --factor común del llamado recursivo. Llama a otra función (sanost)

--Función auxiliar de Ejercicio 3 para parámetro r (individuos recuperados) a los t días
--Cantidad de individuos recuperados aumenta de acuerdo a la tasa de recuperación (g)

recupt :: Float -> Float -> Float -> Float -> Float -> Int -> Float
recupt r0 s0 i0 b g n | n == 0 = r0
                      | otherwise = recupt r0 s0 i0 b g (n-1) + g*(infect s0 i0 b g (n-1)) --llama a otra función (infect)



--Ejercicio 4: Función maxsir devuelve la cantidad de infectados en el día con mayor infectados en n días, partiendo de los parámetros del Ejercicio 3
maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float
maxsir (s0, i0, r0) b g n | n == 0 = i0
                          | infect s0 i0 b g n >= infect s0 i0 b g (n-1) = infect s0 i0 b g n 
                          | otherwise = maxsir (s0, i0, r0) b g (n-1)   

                             