-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería Informática
-- Alumno: MORETON SCHULZ MIGUEL
-------------------------------------------------------------------------------
import Test.QuickCheck

---1)
--a
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = if (x^2)+(y^2)==(z^2) then True else False

--b
terna :: Integer -> Integer -> (Integer,Integer,Integer)
terna x y
          | x > 0 && y > 0 && x > y = ((x^2)-(y^2),2*x*y,(x^2)+(y^2))
          | otherwise = error "No hay terna pitagórica para dichos numeros"           

--c y d
--El antecedente de la propiedad indica que x e y deben ser positivos
--ademas de ser x > y. Dada esa condicion las ternas que generen cualquiera nos Num
--son pitagóricas
p_ternas x y = x>0 && y>0 && x>y ==> esTerna l1 l2 h
  where
    (l1,l2,h) = terna x y


---2)
intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

---3)
--a
ordena2 :: (Ord a) => (a,a) -> (a,a)
ordena2 (x,y)
            | x >= y = (y,x)
            | otherwise = (x,y)

--Dada una tupla con dos ordenables, si se le aplica la funcion "ordena2" el resultado es una tupla cuyos 
--valores estan en orden descendente
p1_ordena2 x y = enOrden (ordena2 (x,y))
  where
    enOrden (x,y) = x<=y


--Dada una tupla con dos ordenables, si se le aplica la funcion "ordena2" el resultado es una tupla cuyos
--valores estan en orden descendente y contiene los mismos ordenables de la tupla original
p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
  where
    mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)


--b
ordena3 :: (Ord a) => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) = (e,f,d)
  where
    (a,b) = ordena2 (x,y)
    (c,d) = ordena2 (b,z)
    (e,f) = ordena2 (a,c)

--c
p1_ordena3 x y z = enOrden (ordena3 (x,y,z))
  where
    enOrden (x,y,z) = x<=y && y<=z

p2_ordena3 x y z = mismosElementos (x,y,z) (ordena3 (x,y,z))
  where
    mismosElementos (x,y,z) (a,b,c) = 
      (x==a && y==b && z==c) || 
      (x==a && y==c && z==b) ||
      (x==b && y==c && z==a) ||
      (x==b && y==a && z==c) ||
      (x==c && y==a && z==b) ||
      (x==c && y==b && z==a)


---4)
max2 :: (Ord a) => a -> a -> a 
max2 x y
        | x >= y = x
        |otherwise = y

--i
p1_max2 x y = enOrden x y (max2 x y)
  where
    enOrden x y max = max==x || max == y

p11_max2 x y = True ==> (max2 x y) == x || (max2 x y) == y

--ii
p2_max2 x y = True ==> (max2 x y) >= x &&  (max2 x y) >= y

--iii
p3_max2 x y = x>=y ==> (max2 x y) == x

--iv
p4_max2 x y = y>=x ==> (max2 x y) == y


---5)
entre :: (Ord a) => a -> (a,a) -> Bool
entre x (y,z)
              | y > z = error "Intervalo invalido"
              | x >= y && x <= z = True
              | otherwise = False


---6)
iguales3 :: (Eq a) => (a,a,a) -> Bool
iguales3 (x,y,z)
              | (x==y) && (y==z) = True
              | otherwise = False


---7)

---a
type TotalSegundos = Integer
type Horas = Integer
type Minutos  = Integer
type Segundos = Integer

descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas, minutos, segundos)
  where
    horas = div (div x 60) 60
    minutos = mod (div x 60) 60
    segundos = mod x 60


--b
p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x 
                            && entre m (0,59)
                            && entre s (0,59)
                              where (h,m,s) = descomponer x


---8)
--a
unEuro :: Double
unEuro = 166.386

pesetasAEuros:: Double -> Double
pesetasAEuros x = x / unEuro

--b
eurosAPesetas :: Double -> Double
eurosAPesetas x = x * unEuro

--c
p_inversas x = eurosAPesetas (pesetasAEuros x) == x
--Falla porque la igualdad es precisa y no aproximada por lo cual
--numeros muy similares son vistos como diferentes 1.9999 /= 2

---9)
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x- y) < epsilon
  where epsilon = 1/1000

p_inversas' x = eurosAPesetas (pesetasAEuros x) ~= x


---10)
discriminante :: Double -> Double -> Double -> Double
discriminante a b c = (b^2)-(4*a*c)

--a
raices :: Double -> Double -> Double -> (Double,Double)
raices a b c
            | discri >= 0 = (x,y)
            | otherwise = error "Raíces no reales"
              where
                discri = discriminante a b c
                x = ((-b)+sqrt(discri)) / 2*a
                y = ((-b)-sqrt(discri)) / 2*a


--b DUDA
p1_raices a b c = esRaiz r1 && esRaiz r2
  where
    (r1,r2) = raices a b c
    esRaiz r = a*r^2 + b*r + c ~= 0

{-
p2_raices a b c = undifine && undifine ==> esRaiz r1 && esRaiz r2
  where
    discri = discriminante a b c
    (r1,r2) = raices a b c
    esRaiz r = a*r^2 + b*r + c ~= 0
-}


---11)
esMultiplo x y = mod x y == 0

--12)
infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
a ==>> b
          | a==False && b==False = True
          | b==True = True
          | a==False = True
          | otherwise = False

--13)
esBisiesto :: Integer -> Bool
esBisiesto x
              | mod x 100 == 0  = mod x 400 == 0
              | mod x 4 == 0    = True
              | otherwise       = False

--14)
--a
potencia :: (Integral a, Eq a) => a -> a -> a
potencia b n
          |  n==0 = 1
          | otherwise = b * potencia b (n-1)

--b
potencia' :: (Integral a) => a -> a -> a
potencia' b n
              | n==0 = 1
              | n==1 = b
              | n==2 = b * b
              | even n    = potencia' (potencia' b (div n 2)) 2
              | otherwise = b * (potencia' (potencia' b (div (n-1) 2)) 2)

--c
p_pot b n = n>=0 ==> potencia b n == sol 
  && potencia' b n == sol
  where sol = b^n

--d DUDA
--potencia = n multiplicaciones si n>0
{-
potencia'
          si n < 1 = 0
          si n > 1
            si n par = n-1 multiplicaciones
            si n impar = n multiplicaciones 

-}

--15)
factorial :: Integer -> Integer
factorial n
            | n == 0 = 1
            | otherwise = n * factorial(n-1)


--16)
--a
divideA :: (Integral a) => a -> a -> Bool
divideA x y
            | mod y x == 0  = True
            | otherwise     = False

--b DUDA
p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x

--c
p2_divideA x y z = x/=0 && divideA x y && divideA x z ==> divideA x (y+z) 

--17)
mediana :: Ord a => (a,a,a,a,a) -> (a,(a,a,a,a,a))
mediana (x,y,z,t,u)
                    | x > z   = mediana (z,y,x,t,u)
                    | y > z   = mediana (x,z,y,t,u)
                    | t < z   = mediana (x,y,t,z,u)
                    | u < z   = mediana (x,y,u,t,z)
                    | otherwise = (z, (x,y,z,t,u))
