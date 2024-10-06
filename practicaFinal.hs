

{- 1. Un numero perfecto es aquel que es igual a la suma de sus divisores menores que el
Ejemplo: 6 : 3, 2, 1

Utilizando lista por compresion, escribir la funcion "perfectosn" que de como resultado
la lista de numeros perfectos comprendidos en el intervalo [1,n]                     -}

sumaDivisores :: (Integral a) => a -> a
perfectosn    :: (Integral a) => a -> [a]
sumaDivisores n = sum [x | x <- [1..n], n `mod` x == 0, x /= n ]
perfectosn p = [x | x <- [1..p], sumaDivisores x == x ]




{- 2. Escribir una funcion que recibe como argumento dos listas ordenadas, y devuelve
una lista ordenada fusion de las listas argumentos (No se debe usar ningun metodo de clasificacion ) -}

uneListas :: (Ord a) => [a] -> [a] -> [a]
uneListas a [] = a
uneListas [] b = b
uneListas (x:xs) (y:ys) = if x < y then (x: uneListas xs (y:ys)) else (y: uneListas ys (x:xs))




{- 3. Escriba una funcion Qsort::(Ord a) => [a] -> [a]. Sin utilizar listas por comprension.

Observacion: Escriba una funcion particion que reciba como argumento, un valor de referencia o pivot y a una lista
de valores del mismo tipo que el pivot. Esta funcion da como resultado una tupla con dos listas (l1, l2) ... de modo que 
en el l1 estan todos los valores de la lista original que son menores o iguales que el pivot, y en l2 todos los mayores que el pivot.

particion :: (Ord a) => a -> [a] -> ([a], [a])      -}

particion :: (Ord a) => a -> [a] -> ([a], [a])
particion p [] = ([], [])
particion p (x:xs) = if x <= p then (x:l1, l2) else (l1, x:l2)
                     where (l1, l2) = particion p xs

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort y ++ [x] ++ qsort ys
                where (y, ys) = particion x xs




{- 4.a) Recordemos que la funcion de la biblioteca ZIP, recibe como argumento dos listas (x:xs) e (y:ys) y produce una lista
de tuplas (i, j) donde los i provienen de la primera lista y los j de la segunda. Cuando una lista es mas larga que la otra, 
el resultado contempla solo los pares hasta donde pudieron formarse... 

EJ: zip [1, 2, 3] [10..] = [ (1, 10), (2, 11), (3, 12) ]

Escriba una version personal de la funcion zip, llamada miZip: miZip :: [a] -> [b] -> [(a,b)]              -}           

miZip :: [a] -> [b] -> [(a,b)] 
miZip _ [] = []
miZip [] _ = []
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys




{- 4. b) Utilizando miZip y listas por comprension, escriba una funcion que realice el producto escalar de dos listas, donde el 
producto escalar estaria definido como la suma de los productos uno a uno, componente a componente de cada lista. Si una lista tuviera
mas elementos que la otra, al agotarse uno de los operandos se detiene a la suma     -} 

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar (x:xs) (y:ys) = sum [ x*y | (x,y) <- miZip xs ys]




{- 4. c) Utilizando solo la funcion miZip, escriba la funcion "indexado". Dada una lista, produce una lista de pares donde cda elemento de la 
lista tiene su posicion dentro de la misma; la indexacion comienza en 1.   -} 

indexado :: [a] -> [(a, Int)]
indexado (x:xs) = miZip (x:xs) [1..] 




{- 5. Escribir una funcion que inserta elementos en una lista de manera de mantenerla ordenada de menor a mayor. De esta forma cada
operacion Head sobre la lista devuelve el elemento mas chico almacenado en ella.   -} 

inserta :: (Ord a) => a -> [a] -> [a]
inserta a [] = [a]
insera a (x:xs) = if a < x then (a:x:xs) else (x: inserta a xs)



{- 6. Consideremos la siguiente funcion:

split :: (Ord a) => a -> [a] -> ([a], [a])
split x l = ( [y | y <- l, y <= x], [y | y <- l, x > y])

-- Defina una version de esta funcion que trabaje en exactamente una sola pasada a la lista l      -}

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:xs:t) = (x:m1, xs:m2)
                 where (m1, m2) = split t




