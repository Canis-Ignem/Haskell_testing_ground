import Data.Char (digitToInt)
-- digitToInt :: Char -> Int

-- Fibonacci eficiente
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibE :: Integer -> Integer
fibE n = fst (fibDos n)
         where
         fibDos :: Integer -> (Integer,Integer)
         fibDos 0 = (0,1)
         fibDos n = let (a,b) = fibDos (n-1)
                         -- a = fib (n-1)
                         -- b = fib n
                    in (b, a+b) 

{-
fibE 3 
   => fst (fibDos 3)
   => fst ((b,a+b) where (a,b) = fibDos 2)
   => fst ((b,a+b) where (a,b) = ((b1,a1+b1) where (a1,b1) = fibDos 1))
   => fst ((b,a+b) where (a,b) = ((b1,a1+b1) 
                                   where (a1,b1) = ((b2,a2+b2) 
                                                    where 
                                                    (a2,b2) = fibDos 0)))
   => fst ((b,a+b) where (a,b) = ((b1,a1+b1) 
                                   where (a1,b1) = ((b2,a2+b2) 
                                                    where 
                                                    (a2,b2) = (0,1))))
   => fst ((b,a+b) where (a,b) = ((b1,a1+b1) 
                                   where (a1,b1) = (1,1)))
   => fst ((b,a+b) where (a,b) = (1,2)) 
   => fst (2,3) 
   => 2   
-}

--EJERCICIO PARA CASA:

ver a = (iterate  (\k -> (k+1) ) a)

inversa [] = []
inversa (x:xs) = inversa xs ++ [x]

inversaE s = invConc [] s
             where 
             invConc t [] = t
             invConc t (x:xs) = invConc (x:t) xs

-- Ejercicio con iterate
digitos = reverse . map (`mod` 10) .  takeWhile (/=0) . iterate (`div` 10) 

-- La criba de Eratostenes
primos = criba [2..]
criba (p : ns) = p : criba (filter ((/= 0).(`mod`p)) ns)

-- take 100 (filter ((==7).(`mod` 10)) primos)
-- (filter ((==7).(`mod` 10)) primos) !! 99


-- Estructura cíclicas
data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a) deriving Show

ca = let x =(Nodo (Nodo ca (Hoja 5)) x) 
     in Nodo x (Hoja 6)

ca' = let x =(Nodo (Nodo ca (Hoja 5)) x) 
      in Nodo (Hoja 6) x

unfold:: (Show a1, Eq a2, Num a2) => a2 -> Arbol a1 -> Arbol String
unfold _ (Hoja r) = Hoja (show r)
unfold 0 (Nodo ai ad) = Hoja " "
unfold n (Nodo ai ad) = Nodo (unfold (n-1) ai)  (unfold (n-1) ad)

-- repeat
-- [1,3,1,3,1,3,.....]
lista = concat (repeat [1,3])

--ISBN (http://www.isbn-check.com/)
{-
978-18-6197-271-2

978186197271

9 x 1 = 9
7 x 3 = 21
8 x 1 = 8
1 x 3 = 3
8
6
1
9
7
2
7
1
    + -----
       118 = s
Si (s mod 10) = m es cero, el 13-ésimo dígito del ISBN debe ser cero.
En caso contrario, el 13-ésimo dígito del ISBN debe ser 10-m.
-}

isbn:: String -> Bool
isbn s = correctLength && correctFormula
         where
         singuiones = filter (/= '-') s
         correctLength = (length singuiones == 13)
         listDigit = map digitToInt singuiones
         total = sum(zipWith (*) (cycle [1,3]) (init listDigit))
         mod10 = mod total 10
         ultDigit = last listDigit
         correctFormula = if mod10 == 0 then ultDigit == 0
                          else ultDigit == 10-mod10
-- cycle = concat . repeat   

-- Lista infinita de los números de Fibonacci                      

fibs = map fst (iterate (\(a,b)->(b,a+b)) (0,1))

-- iterate f x = zs where zs = x : map f zs

-- Números de Hamming               
hamming :: [Integer] 
-- hamming es la lista infinita creciente de los números de Hamming
hamming = 1: fundir3 (map (2*) hamming)
                     (map (3*) hamming)
                     (map (5*) hamming)
          where fundir3 :: Ord a => [a] -> [a] -> [a] -> [a] 
                -- Pre: xs ys y zs son listas ordenadas crecientemente
                -- (fundir3 xs ys zs) es la lista ordenada creciente 
                --  sin repeticiones que resulta de mezclar xs, ys y zs
                fundir3 xs ys zs = fundir2 xs (fundir2 ys zs)
                fundir2 (x:xs) (y:ys)
                    | x == y = x: fundir2 xs ys     
                    | x < y  = x: fundir2 xs (y:ys)
                    | x > y  = y: fundir2 (x:xs) ys

{-
*Main> hamming !! 10000000
16244249195502759967226308067328000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
(15.08 secs, 9,476,121,264 bytes)
-}

-- EJERCICIO: Dada una cantidad c y un %x de interés anual, 
-- generar la lista infinita de cantidades en que se convierte
-- c cada año.
-- Utilizar esa lista para calcular, dados c, %x, y una cantidad o,
-- en cuantos años la cantidad será >= que o.

--anuales :: Fractional a => a -> a -> [a]
--anuales c x = iterate (\k-> k + (x*k/100)) c

añosNec :: (Ord a, Fractional a) => a -> a -> a -> Int
añosNec c x obj = length (takeWhile (< obj) (anuales c x))
                  where anuales c x = iterate (\k-> k + (x*k/100)) c