module Test where

--- Funciones:

divi :: Int -> Int -> Int   -- Prototipo de la funcion
divi x y = x `div` y        -- Funcion real

doblame :: Int -> Int       -- Toma un entero y devuelve un entero 
doblame x = x + x

redoblame :: Int -> Int -> Int
redoblame x y = doblame x + doblame y       -- La funcion og siempre con =, los prototipos con :: 


a = [1, 2, 3, 4, 69]        -- Lista (Unico tipo)
b = ("hola", ('a', 34), 65) -- Tupla (Varios tipos, incluso otra tupla anidada)
c = [10]                    -- Unica forma de concatenar un elemento a una lista (osea concat es solo para listas)

-- Concatenar listas: a ++ c = [1, 2, 3, 4, 69, 10]


signos :: Int -> [Char]        -- Para pasar negativos en PARENTESIS pq si no lo toma con resta   ( seria (-1))
signos n = if n < 0 then "Negativo" else
           if n == 0 then "Zero" else "Positivo"


-- Pattern Matching
-- Func       Class        Parametros
factorial :: (Integral a) => a -> a
factorial 0 = 1                         -- Primer patron o pattern, donde si se llega a 0, se convierte en 1 y para
factorial n = n * factorial (n-1)       -- Segundo, cuerpo de la funcion, donde calculara el factorial del numero n

-- Aqui vimos algo de recursion, donde se multiplica a n por el factorial de n-1, hasta llegar a 0



--- Contar Elementos en Lista:

cuenta :: [a] -> Int    -- Cuenta cualquier cosa "a"
cuenta [] = 0               -- Condicion de Finalizacion ( cuando la lista se quede sin elementos o este vacia )
cuenta (x:t) = 1 + cuenta t

--- La cuestion es que la funcion se aplica a cada elemento, donde X saca el primer elemento y t es el resto de la lista; en cada llamada 
--- x saca 1 elemento y queda la cuenta t ----> [8, 5, 6] -->   x:8   y   t:[5, 6]   y despues sigue   x:5 y t:[6]


-- Ejemplo de _ y constructor con pattern matching

tell :: (Show a) => [a] -> String
tell []       = "La lista está vacía"
tell (x:[])   = "La lista tiene un elemento: " ++ show x
tell (x:y:[]) = "La lista tiene dos elementos: " ++ show x ++ " y " ++ show y
tell (x:y:_)  = "La lista es larga. Los primeros dos elementos son: " ++ show x ++ " y " ++ show y


-- Uso de guardas por patrones: 

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Tienes infrapeso ¿Eres emo?"
    | weight / height ^ 2 <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
    | weight / height ^ 2 <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise                   = "¡Enhorabuena, eres una ballena!"



-- Sucesion de Fibonacci

fibonacci :: Int -> [Int]
fibonacci n = take n (fibSeq 0 1)

fibSeq :: Int -> Int -> [Int]
fibSeq a b = a : fibSeq b (a + b)