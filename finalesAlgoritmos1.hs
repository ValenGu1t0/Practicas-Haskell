module Finales where

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




-------------------------------------- TAD ---------------------------------------



{- 1. ARBOL BINARIO

Defina un tipo de dato arbol binario de busqueda. Escriba el metodo addTree e inOrderTree, donde:

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a --- Inserta un elemento del tipo a en un arbol binario

inOrderTree :: (Ord a) => ArbolBin a -> [a] --- Produce un listado "En orden" del arbol binario

El listado en orden del arbol se define de la siguiente manera, primero se lista en orden al arbol izquierdo, luego la raiz y finalmente
se lista en orden el arbol derecho.       -}


data ArbolBin a = VacioAB | NodoAB a (ArbolBin a) (ArbolBin a) deriving Show

mkNewTree   :: (Ord a) => ArbolBin a
addTree     :: (Ord a) => a -> ArbolBin a -> ArbolBin a 
surfTree    :: (Ord a) => a -> ArbolBin a -> Bool
inOrderTree :: (Ord a) => ArbolBin a -> [a]

mkNewTree = VacioAB

addTree a VacioAB = NodoAB a VacioAB VacioAB
addTree a (NodoAB n izq der) | a == n = NodoAB n izq der
                             | a < n = NodoAB n (addTree a izq) der
                             | a > n = NodoAB n izq (addTree a der)

surfTree a VacioAB = False 
surfTree a (NodoAB n izq der)| a == n True
                             | a < n = surfTree a izq
                             | a > n = surfTree a der

inOrderTree VacioAB = []
inOrderTree (NodoAB n izq der) = inOrderTree izq ++ [n] ++ inOrderTree der






{- 2. Una cola de prioridad es una estructura de datos que almacena elementos "clasificables", con la particularidad que, cuando se saca
uno de ella siempre se extrae el elemento con menor clave, de ahi su nombre pues clasifica los elementos en funcion de su prioridad.
La prioridad mas baja primero. Las funciones que manipulan a la cola de prioridad son:

mkqpr: Instancia una nueva cola de prioridad vacia
addqpr: Agrega un nuevo elemento a la cola de prioridad
nextqpr: Devuelve un elemento con clave mas baja de la cola de prioridad
popqpr: Devuelve una cola de prioridad donde se ha quitado el nextqpr

Defina el TAD ColaPrioridad, e implemente el mismo utilizando un arbol binario de busqueda como estructura de almacenamiento.
Escribir todas las funciones necesarias para la manipulacion de la estructura subyascente, es decir para manipular el arbol.
Sugerencia: recordar como extraer el elemento con clave mas pequeña de un arbol            -}



-- Definición del tipo de dato para el árbol binario de búsqueda
data ArbolBinario a = Vacio | Nodo a (ArbolBinario a) (ArbolBinario a) deriving (Show)

-- Definición del TAD ColaPrioridad
data ColaPrioridad a = CP (ArbolBinario a) deriving (Show)

-- Función para instanciar una nueva cola de prioridad vacía
mkqpr :: ColaPrioridad a
mkqpr = CP Vacio

-- Función auxiliar para insertar un elemento en un árbol binario de búsqueda
insertar :: (Ord a) => a -> ArbolBinario a -> ArbolBinario a
insertar elemento Vacio = Nodo elemento Vacio Vacio
insertar elemento (Nodo valor izq der)
  | elemento <= valor = Nodo valor (insertar elemento izq) der
  | elemento > valor = Nodo valor izq (insertar elemento der)

-- Función para agregar un elemento a la cola de prioridad
addqpr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a
addqpr elemento (CP arbol) = CP (insertar elemento arbol)

-- Función auxiliar para encontrar el elemento con clave más baja en un árbol binario de búsqueda
buscarMinimo :: ArbolBinario a -> Maybe a
buscarMinimo Vacio = Nothing
buscarMinimo (Nodo valor Vacio _) = Just valor
buscarMinimo (Nodo _ izquierda _) = buscarMinimo izquierda

-- Función para devolver el elemento con la clave más baja de la cola de prioridad
nextqpr :: (Ord a) => ColaPrioridad a -> Maybe a
nextqpr (CP arbol) = buscarMinimo arbol

-- Función auxiliar para eliminar el elemento con clave más baja de un árbol binario de búsqueda
eliminarMinimo :: (Ord a) => ArbolBinario a -> ArbolBinario a
eliminarMinimo Vacio = Vacio
eliminarMinimo (Nodo valor Vacio derecha) = derecha
eliminarMinimo (Nodo valor izquierda derecha) = Nodo valor (eliminarMinimo izquierda) derecha

-- Función para devolver una cola de prioridad donde se ha quitado el elemento con clave más baja
popqpr :: (Ord a) => ColaPrioridad a -> ColaPrioridad a
popqpr (CP arbol) = CP (eliminarMinimo arbol)





{- 3. Un conjunto o SET es una coleccion de items del mismo tipo distingibles entre si por su clave o valor,
en la cual un item puede ser testeado si es miembro, insertado o borrado de la coleccion. La cantidad de elementos
distintos es lo que se denomina el tamaño del conjunto.

Defina el tipo de dato e implemente los metodos del nuevo tipo de daro, utilizando listas no ordenadas y sin duplicados. 
El metodo unionSet (union de dos conjuntos) se escribira haciendo uso de los metodos ya definidos, es decir, no se 
operara directamente la lista sino se operara al SET.

-}

Module Set (Set, emptySet, setEmpty, inSet, addSet, delSet) where

emptySet :: Set a 
setEmpty :: Set a -> Bool
inSet    :: (Eq a) => a -> Set a -> Bool
addSet   :: (Eq a) => a -> Set a -> Set a
delSet   :: (Eq a) => a -> Set a -> Set a
unionSet :: (Eq a) => Set a -> Set a -> Set a 

emptySet = Set []

setEmpty (Set []) = True
setEmpty (Set [x]) = False

inSet x (Set []) = False
inSet x (Set (y:ys)) = x == y || inSet x (Set (ys))

addSet x (Set xs) = if inSet x (Set (xs)) then Set (xs) else addSet (Set x:xs)

delSet x (Set []) = Set []
delSet x (Set (y:ys)) = if x /= y then addSet y (delSet x (Set (ys))) else delSet x (Set (ys))

unionSet (Set []) (Set (x)) = Set (x)
unionSet (Set (x:xs)) (Set (y)) = if inSet x (Set y) then unionSet (Set (xs)) (Set (y)) else unionSet (Set(xs)) (Set (x:y))







Module Set (Set, emptySet, setEmpty, inSet, addSet, delSet) where

emptySet :: Set a 
setEmpty :: Set a -> Bool
inSet    :: (Eq a) => a -> Set a -> Bool
addSet   :: (Eq a) => a -> Set a -> Set a
delSet   :: (Eq a) => a -> Set a -> Set a
unionSet :: (Eq a) => Set a -> Set a -> Set a 

emptySet = Set []

setEmpty (Set []) = True
setEmpty (Set [x]) = False

inSet x (Set []) = False
inSet x (Set (y:ys)) = x == y || inSet x (Set (ys))

addSet x (Set xs) = if inSet x (Set (xs)) then Set (xs) else addSet (Set x:xs)

delSet x (Set []) = Set []
delSet x (Set (y:ys)) = if x /= y then addSet y (delSet x (Set (ys))) else delSet x (Set (ys))

unionSet (Set []) (Set (x)) = Set (x)
unionSet (Set (x:xs)) (Set (y)) = if inSet x (Set y) then unionSet (Set (xs)) (Set (y)) else unionSet (Set(xs)) (Set (x:y))