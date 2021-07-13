-- Ejemplos en las tranparencias
import System.Directory.Internal.Prelude

raices:: (Float,Float,Float) -> (Float,Float)
raices (a,b,c) 
       | a == 0 = error "no es cuadrática"
       | e < 0 = error "raices complejas"
       | otherwise = ((-b-s)/d, (-b+s)/d)
                      where 
                        e = b*b - 4*a*c
                        s = sqrt e
                        d = 2*a

positLista:: [Int] -> [Int]
positLista [] = []
positLista (x:s) = if x > 0 then x: (positLista s)
                            else positLista s
                            
minimoc :: Int -> Int -> Int
minimoc x y 
    | x <= y     = x
    | otherwise  = y
 
--1.-

quitaUno:: (Eq a) => a -> [a] -> [a]
quitaUno _ [] = []
quitaUno x (y:ys) = if x == y then ys else y : quitaUno x ys
-- Haskell no permite:
--quitaUno x (x:ys) = ys
--quitaUno x (y:ys) = quitaUno xs ys

-- Version no-currificada
quitaUno':: (Eq a) => (a,[a]) -> [a]
quitaUno' (_,[]) = []
quitaUno' (x,(y:ys)) = if x == y then ys else y : quitaUno'(x,ys)

-- 2.-

quitarRep :: Eq a => [a] -> [a]
quitarRep [] = []
quitarRep (x:xs) = x: quitarRep (quitarTodos x xs)
                   where 
                   quitarTodos _ [] = []
                   quitarTodos z (y:ys) 
                       = if z == y then quitarTodos z ys  
                         else y : quitarTodos z ys

quitarRep' :: Eq a => [a] -> [a]
quitarRep' [] = []
quitarRep' (x:xs) = if x `elem` xs then quitarRep' xs 
                                   else x : quitarRep' xs

-- 3.-
dif:: (Eq a) => [a] -> [a] -> [a]
dif xs [] = xs
dif xs (y:ys) = dif (quitaUno y xs) ys

-- 4.-
perm:: (Eq a) => [a] -> [a] -> Bool
perm xs ys = (dif xs ys == []) && (dif ys xs == [])

-- 5.- sin usar filter
{-
sonpermde1:: (Eq a) => [[a]] -> [[a]]
sonpermde1 [] = []
sonpermde1 (x:xs) = x : sonperm x xs    
                    where 
                    sonperm:: (Eq a) => [a] -> [[a]] -> [[a]]
                    sonperm y [] = []
                    sonperm y (z:zs) = if perm z y then z: sonperm y zs                   else sonperm y zs
-}

-- 5.- usando perm y filter

sonpermde1:: (Eq a) => [[a]] -> [[a]]
sonpermde1 [] = []
sonpermde1 (x:xs) = x : filter (perm x) xs 

-- 6.

aDecimal:: [Int] -> Int
aDecimal = --let g x y = 10*x+y in foldl g 0  -- usando foldl
           let g x y = 10*x+y in foldl1 g  -- usando foldl1

    
aDigitos:: Int -> [Int]
aDigitos n
     | 0 <= n && n <= 9 = [n]
     | otherwise        = aDigitos (n `div` 10) ++ [n `mod` 10]

-- 7.
decimalAbinario:: Int -> Int
decimalAbinario = aDecimal . dAb
                  where 
                  dAb:: Int -> [Int]
                  dAb x
                     | x < 0            = error "numero negativo"
                     | 0 <= x && x <= 1 = [x]
                     | otherwise        = dAb (div x 2) ++ [mod x 2]

binarioAdecimal:: Int -> Int
binarioAdecimal = (foldl1 xpor2masy) . aDigitos            
                  where xpor2masy x y = x*2+y

-- 8.- Ejercicio para casa



{-
estaOrdenada ::(Enum a,Eq a) =>  [a] -> Bool
estaOrdenada (x:y:xs)
            |isDigit1 x =  if  x < y
                                then estaOrdenada (y:xs)
                                else False
            |isAlpha1 x =  if fromEnum x < fromEnum y
                          then estaOrdenada (y:xs)
                          else False
            |otherwise =  error "Tipo inadecuado"
            
-}

estaOrdenada ::(Ord a) =>  [a] -> Bool
estaOrdenada []= True
estaOrdenada (x:[]) = True
estaOrdenada (x:y:xs) = x <= y && estaOrdenada (y:xs)

          

-- 9.- 
palabras:: String -> [String]
{-
palabras s =
     if s' == "" then []
     else (tomarPal1 s') : (palabras (saltarPal1 s'))
       where
       s' = saltarBlancos s
       saltarBlancos = dropWhile (== ' ')
       tomarPal1 = takeWhile (/= ' ')
       saltarPal1 = dropWhile (/= ' ')
-}

palabras s
 = case dropWhile (== ' ') s of
        "" -> []
        s' -> pal1 : palabras resto
              where (pal1,resto)= span (/= ' ') s'
                       --break (== ' ') s'
                       -- (takeWhile (/=' ') s', dropWhile (/=' ') s')
                       
{-
palabras s = case dropwhile (== ' ') s of
              "" -> []
              s' -> w : palabras s''
              where (w,s'')= --break (== ' ') s'
                             span (/= ' ') s'
                           -- (takeWhile (/=' ') s', dropWhile (/=' ') s')
-}

-- Ejercicio genQsort (transparencias)

quicksort :: (Ord a) => [a] -> [a]
{-
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <  x] ++
                   [x] ++
                   quicksort [y | y <- xs, y >=  x]
-}

quicksort [] = []
quicksort (x:xs) = quicksort (filter (< x) xs) ++
                   [x] ++
                   quicksort (filter (>= x) xs)

genQsort :: (a -> a -> Bool) -> [a] -> [a]
genQsort p [] = []
genQsort p (x:xs) = genQsort p [y | y <- xs, p y x] ++
                    [x] ++
                    genQsort p [y | y <- xs, not (p y x)]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' = genQsort (<)

quicksortD :: (Ord a) => [a] -> [a]
quicksortD = genQsort (>)

quicksortPares :: (Ord a,Ord b) => [(a,b)] -> [(a,b)]
quicksortPares = genQsort (\(x,y) (u,v) -> x < u && y > v)

-- Ejercicios listas intensionales

--1.- 
intersec :: (Eq a) => [a] -> [a] -> [a]
intersec xs ys = -- [x | x <- xs, elem x ys]
                 filter (\x -> elem x ys) xs
                 
--2.-
numveces :: (Eq a) => a -> [a] -> Int
--numveces x xs = length [y | y <- xs, y == x]
numveces x = length . (filter (\y -> y == x))

--3.-
mismosElem :: (Eq a) => [a] -> [a] -> Bool
mismosElem xs ys = and [elem x ys | x <- xs] &&
                   and [elem y xs | y <- ys]
                   
--4.-
posiciones :: (Eq a) => a -> [a] -> [Int]
posiciones x xs = [p | (e,p) <- zip xs [0..], e == x]

--5.-
posimpar :: [a] -> [a]
posimpar xs  = map fst (filter (odd.snd) (zip xs [0..]))
-- posimpar xs = [xs !! i | i <- [1,3 .. lenght xs - 1]]

--6.-
todosIg :: (Eq a) => [a] -> Bool
todosIg xs = and [ x == y | (x,y) <- zip xs (tail xs)]

--7.-
happyFreq :: [(Char,Int)]
happyFreq = [(x,c) | x <-['a'..'z'], 
                     let c = (length.filter (== x)) "happy",
                     c > 0]
                     
freqList :: String -> [(Char,Int)]
freqList s 
  = (sumar.agrupar) (zip s [1 | i <-[0..]])
     where 
       agrupar = genQsort (\(x,y) (u,v) -> (x == u))
       sumar = foldr op []
       op :: (Char,Int) -> [(Char,Int)] -> [(Char,Int)]
       --op (c1,f1) [] = [(c1,f1)]
       --op (c1,f1) ((c2,f2):resto) 
       --   = if c1 == c2 then (c1,f1+f2):resto
       --                 else (c1,f1): ((c2,f2):resto)
       op = \(c1,f1) ps -> if ps /= [] && c1 == (fst.head) ps 
                           then (c1,f1+(snd.head)ps):(tail ps)
                           else (c1,f1): ps                
                        
                        
--Ejercicio: 1.- Mejorar para que sólo saque las letras.
--           2.- Mejorar para que cuente mayúsculas y minúculas juntas.
 
poner :: a -> [a] -> [[a]]
poner x [] = [[x]]
poner x (y:ys) = (x:y:ys) : map (y:) (poner x ys)
-- 8.- Ejercicio 12 de la hoja 2
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) 
  = concat (map (poner x) (permutaciones xs))
  {-
    where
     poner :: a -> [a] -> [[a]]
     poner x [] = [[x]]
     poner x (y:ys) = (x:y:ys) : map (y:) (poner x ys)
     -}
     
perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms p = [x:xs | x <- p, xs <- perms (delete x p)]
          where
          delete x xs = (takeWhile (/=x) xs) ++ tail (dropWhile (/=x) xs)         
-- Ejercicio: Usar span para recorrer xs una sóla vez  

--9.-
triads :: Int -> [(Int,Int,Int)]

triads n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n],
                      x^2 + y^2 == z^2]

-- Evitando la conmutativa de la suma, 
-- es decir, evitando que aparezcan (x,y,z) e (y,z,x)                  
triadsxLEy n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [1..n],
                          x^2 + y^2 == z^2]

-- Evitando generar los z's imposibles                          
triadsxLEyLTz n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y+1..n],
                             x^2 + y^2 == z^2]

-- Usando que hay un teorema que asegura que si x e y son los dos impares
-- no existe tal z.                          
triadsOdd n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y+1..n],
                         mod x 2 /= 1 || mod y 2 /= 1,
                         x^2 + y^2 == z^2]
-- Esta última función es computacionalmente más cara que la anterior,
-- por el test adicional que comprueba en todos los casos.

---------------------------------------------------------------------------
                
-- Ejercicio:
-- Un núm. es perfecto si es igual a la suma de todos sus divisores excepto el mismo.
-- Diseñar una función que obtenga la lista de todos los números perfectos 
-- menores o iguales que un n dado.

-- Conjetura de Collatz (Collatz Conjecture).
-- Mirar en la web en que consiste la conjetura.
-- La cadena de Collatz para el número inicial 1 es: 1, 4, 2, 1.
-- y para 6 es: 6, 3, 10, 5, 16, 8, 4, 2, 1. 
-- y para 11 es: 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1.
-- La cadena de numero inicial 1 tiene longitud 4, 
-- la del 6 es de longitud 9, y la del 11 es de longitud 15.
-- Si probais que toda cadena de Collatz siempre llega a alcanzar el 1, os haréis famosos/as,
-- pero ese no es el ejercicio, sino:
-- Diseñar una función que dados x, y, n, obtenga todas las cadenas de Collatz de
-- longitud (exactamente) n, cuyos números iniciales están entre x e y (puede suponerse x<=y).
