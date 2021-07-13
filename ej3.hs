--type Fecha = (Int,Int,Int) -- tipo sinónimo

newtype Fecha = F(Int,Int,Int) --tipo nuevo
                deriving Eq -- ,Show)
                --,Ord No da el orden en fechas como lo quiero

instance Ord Fecha where
    F(d,m,a) <= F(d',m',a') = (a,m,d) <= (a',m',d')
    
-- Ejercicio: Utilizar una función de tipo fold para definir una 
-- función que dada una lista de Fechas obtenga la fecha más temprana. 

fechaMasTemp :: [Fecha] -> Fecha
fechaMasTemp [] = error "no hay ninguna fecha"
--fechaMasTemp [f] = f --sobra
fechaMasTemp fs = foldr1 min fs

--Ejercicio: mostrar la fecha usando una funcion auxiliar showmes
--que muestre Enero, Febrero, Marzo,  etc

instance Show Fecha where
    show (F(d,m,a)) = 
       show d ++ " de " ++ showmes m ++ " de " ++ show a
       where showmes m = case m of 
                           1 -> "Enero"
                           2 -> "Febrero"
                           3 -> "Marzo"
-- Ejercicio: acabar la definición
       
---- Racional

data Racional = Int :/ Int -- deriving Show
                           -- deriving Eq

-- Ejercicio: Definir Racional como una instancia de la clase Eq,
-- tal que por ejemplo 1/2 == 2/4 == 3/6,  etc.

instance Eq Racional where 
  (x:/y) == (x':/y') = x*y' == y*x'

-- Ejercicio: Racional instancia de la clase Ord 
-- tal que p.e. 2/5 < 2/4
  
instance Ord Racional where 
  (x:/y) <= (x':/y') = x*y' <= y*x'
  
sumaRac :: Racional -> Racional -> Racional
sumaRac (x1:/y1) (x2:/y2) = (x1*y2 + x2*y1) :/ (y1*y2)

-- Ejercicio: Hacer una instancia de Show para el tipo Racional
              -- que muestre 1:/2 como 1/2
              -- y muestre 25:/25 como 1
              -- y muestre 2:/4 como 1/2

instance Show Racional where
  show (x:/y) = if denominador == 1 then show numerador
                else (show numerador) ++ "/" ++ (show denominador)
                 where
                 z = gcd x y
                 numerador = div x z
                 denominador = div y z
  
maxListRac:: [Racional] -> Racional
maxListRac = foldr1 max

------ EJERCICIO PARA CASA (pag 83)
type Direccion = (String,Int) --tipo sinónimo

-- esta sería la función que equivale al show
-- si hicieramos un tipo nuevo instancia de la clase Show
escribirDir:: Direccion -> String
escribirDir (c,n) = "Calle " ++ show c ++ ", " ++ show n


--newtype Direc = D(String,Int) 

--instance Show Dired where  
--     show D(c,n) = 

---------
-- Class Num - minimal complete definition:
-- (+), (*), abs, signum, fromInteger, (negate or (-))

newtype Natural a = Nat a deriving Show

toNatural :: (Integral a) => a -> (Natural a)
toNatural x | x < 0 = error "No hay naturales negativos"
            | otherwise = Nat x

instance (Integral a) => Num (Natural a) where
-- (+), (-), (*), abs, signum, fromInteger.
   (Nat x) + (Nat y) = if x>=0 && y>=0 then Nat (x+y)
                       else error "Suma con natural negativo"
   (Nat x) - (Nat y) = if x>=0 && y>=0 then toNatural (x-y)
                       else error "Resta con natural negativo"
   (Nat x) * (Nat y) = if x>=0 && y>=0 then Nat (x*y)
                       else error "Producto con natural negativo"
   abs (Nat x) = toNatural x
   -- signum :: Num a => a -> a
   signum (Nat x) | x < 0 = error "Signo de natural negativo"
                  | x == 0 = 0    -- (Nat 0)
                  | otherwise = 1 -- (Nat 1)
   --fromInteger :: Num a => Integer -> a --tipo general
   --fromInteger :: (Integral a) => Integer -> (Natural a)
   --fromIntegral :: (Integral a, Num b) => a -> b
   --toNatural :: (Integral a) => a -> (Natural a)
   fromInteger = toNatural . fromIntegral
   
fromNatural :: (Integral a) => (Natural a) -> a
fromNatural (Nat i) = i
     
instance (Integral a) => Enum (Natural a) where
-- minimal complete definition: toEnum, fromEnum
   toEnum = fromIntegral
   fromEnum = fromIntegral . fromNatural
   
-- Ejercicio sobre usar cambio de clase de números

-- length :: Foldable t => t a -> Int
-- length :: [a] -> Int
-- (/) :: Fractional a => a -> a -> a
-- sum :: Num a => [a] -> a

-- NO (Fractional Int)
--fromIntegral :: (Integral a, Num b) => a -> b

--media :: (Fractional a, Foldable t) => t a -> a
media :: (Fractional a) => [a]-> a
--media :: (Fractional a) => [Int] -> a
media xs = sum xs / fromIntegral (length xs)



-- EJERCICIO PARA CASA:
  
newtype Point = P (Int,Int) deriving (Eq, Show)
    
a = P (0,0)
b = P (25,100)
c = P (75,100)
d = P (100,0)

-- Evaluar a,b,c, y d.
-- Hacer instancia de Show (quitar Show de deriving) que muestre 
-- por ejemplo c como (X:75,Y:100) en vez de como P (75,100)

-- Función que calcule la distancia entre dos puntos
-- distancia((x1,y1),(x2,y2))  = sqrt((x2-x1)^2 + (y2-y1)^2)

-- Función que decida si tres puntos son los vértices de un triangulo rectángulo
-- Recíproco del teorema de Pitágoras: Si en un triángulo el cuadrado del lado mayor 
-- es igual a la suma de los cuadrados de los otros dos lados entonces el triángulo es rectángulo