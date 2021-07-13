import System.Directory.Internal.Prelude
doble:: Int -> Int
doble x =x + x

sumdo :: Int -> Int -> Int
sumdo x y = x + doble y

area :: Float -> Float
area r = pi * r^2

perimetro :: Float -> Float
perimetro r = 2 * pi * r

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

agregar :: Int -> Int -> Int
agregar x y 
    |0 <= y && y <=9 = 10*x+y
    |otherwise = error "y no es un digito"

sumCuad:: Int -> Int -> Int -> Int
sumCuad x y z = a^2 + b^2
                where 
                    (a,b) = dosMax3 x y z
dosMax3:: Int -> Int -> Int -> (Int , Int)
dosMax3 x y z
    | x <= y && x <= z = (y,z)
    | y <= x && y <= z = (x,z)
    | otherwise        = (x,y)

dosMaxAlt:: Int -> Int -> Int -> (Int, Int)
dosMaxAlt x y z = (0,0) --Pendiente

divModA:: (Int, Int) -> (Int, Int)
divModA (x,y) = (div x y, mod x y)

divModB:: (Int, Int) -> (Int, Int)
divModB (x,y) = (a, b)
                where
                    a = div x y
                    b = mod x y

edad:: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (a,b,c) (x, y, z)
    | b < y || (b == y && a <= x) = z -c
    | c > z = error "Datos errones"
    | otherwise  = z - c -1

sigLetra:: Char -> Char
sigLetra x
    |not(isAlpha x) = error "No es una letra"
    |x == 'Z' = 'Z'
    |x == 'z' = 'z'
    |otherwise = toEnum(fromEnum x +1)


prod:: Int -> Int -> Int
prod n m
    | n == m = n
    | n < m = n * prod (n+1) m
    | m < n = 1
    | otherwise = error "Datos erroneos"


import Data.Char -- Esto es necesario en Haskell 8.6.3 para importar isAlpha y otras funciones sobre Char

doble:: Int -> Int
doble x = x + x

sumdo:: Int -> Int -> Int 
sumdo x y = x + doble y

sumdo' (x,y) = x + doble y

lis = map (sumdo 3) [5,4,2]

f x = 3 > (x `mod` 2)

area:: Float -> Float
-- area r = pi * r * r
area = (pi *) . (^2)
{-
area 2
--> ((pi *) . (^2)) 2
--> (pi *)(2^2)
--> pi * 2^2
--> pi * 4
--> 12.566371
-}

perimetro:: Float -> Float
-- perimetro r = 2 * pi * r
perimetro = (*2).(*pi)

agregar:: Int -> Int -> Int
agregar x y 
       | 0 <= y && y<= 9 = 10*x + y 
       | otherwise = error "y no es un dígito"
  
sumcuad:: Int -> Int -> Int -> Int
sumcuad x y z = a^2 + b^2
                 where 
                  (a,b) = dosMay x y z
                  dosMay:: Int -> Int -> Int -> (Int,Int)
                  dosMay x y z
                     | x <= y && x <= z = (y,z)
                     | y <= x && y <= z = (x,z)
                     | otherwise        = (x,y)

-- variante, ejercicio para casa: que sume el cuadrado del mayor (de los dos mayores) con el cubo del 
-- mas pequeño (de los dos mayores).

myDivMod:: (Int,Int) -> (Int,Int)
myDivMod (x,y) = (div x y, mod x y)

sigLetra:: Char -> Char
sigLetra x
     | not (isAlpha x)  = error "x no es una letra"
     | x == 'Z'         = 'A'
     | x == 'z'         = 'a'
     | otherwise        = toEnum (fromEnum x + 1)

-- Ejercicio 8, para casa, usando fromEnum

prod:: Int -> Int -> Int
-- asumimos n <= m
prod n m
     | n == m = n
     | n < m  = n * prod (n+1) m
     | otherwise = error "datos incorrectos"

edad:: (Int,Int,Int) -> (Int,Int,Int) -> Int
edad (dp,mp,ap) (d,m,a)
-- (dp,mp,ap) es la fecha de nacimiento
-- (d,m,a) es la fecha de hoy
     | mp < m || (mp == m && dp <= d)  = a-ap
     | otherwise                       = a-ap-1
