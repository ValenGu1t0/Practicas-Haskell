-------------------------------------- TAD ---------------------------------------

-- Tipos de Datos Abstractos y como los resolví


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





{- 2. COLA DE PRIORIDAD

Una cola de prioridad es una estructura de datos que almacena elementos "clasificables", con la particularidad que, cuando se saca
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







{- 3. SET 

Un conjunto o SET es una coleccion de items del mismo tipo distingibles entre si por su clave o valor,
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