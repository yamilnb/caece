--Ejercicio 1
signo :: Int -> Int
signo 0 = 0
signo a 
    | a > 0 = 1
    | a < 0 = (-1)
    
negativo :: Int -> Bool
negativo a = if signo (a) == (-1) then True else False

--Ejercicio 2
max :: Int -> Int -> Int
max a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = Main.max c ((Main.max a) b)

min :: Int -> Int -> Int
min a b = if ((Main.max a) b) == a then b else a

--Ejercicio 3
factorial :: Double -> Double
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)

combinatorio :: Double -> Double -> Double
combinatorio n x = (factorial n) / (*) (factorial x)  (factorial (n-x))

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n1 = fibonacci (n1-1) + fibonacci (n1-2)

divisiblePor :: Int -> Int -> Bool
divisiblePor a b = if (mod a b) == 0 then True else False

--Ejercicio 4
esVacia :: [a] -> Bool
esVacia [] = True
esVacia (_:_) = False

cabeza :: [a] -> a
cabeza [] = error "Lista vacía"
cabeza (h:c) = h

resto :: [a] -> [a]
rest [] = error "Lista vacía"
resto (h:c) = c

--Ejercicio 5
long :: [a] -> Int
long [] = 0
long (h:c) = 1 + long c

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (h:c) = h + sumaLista c

-- Se puede aplicar para cualquier tipo de lista cuyo elementos se puedan comprar
member :: [Int] -> Int -> Bool
member [] a = False
member (h:c) a = if (h == a) then True else member c a

append :: [a] -> [a] -> [a]
append [] b = b
append (h:c) b = (h:((append c)b)) 

tomar :: Int -> [a] -> [a]
tomar 0 a = []
tomar a [] = []
tomar a (h:c) = (h:(tomar (a-1) c))

term :: [a] -> Int -> a
term (h:c) 1 = h
term (h:c) a = term c (a-1)

rev :: [a] -> [a]
rev [] = []
rev (h:c) = append (rev c) [h]

maxl :: [Int] -> Int
maxl (h:[]) = h
maxl (h:(h2:[])) = Main.max h h2
maxl (h:c) = Main.max h (maxl(c))

cuenta :: Eq a => a -> [a] -> Int
cuenta x [] = 0
cuenta x (h:c)
    | x == h = 1 + cuenta x c
    | x /= h = 0 + cuenta x c

repite :: Int -> a -> [a]
repite 1 b = (b:[])
repite a b = (b: (repite (a-1) b))

--Ejercicio 6: Resolución en clase
(<->) :: Int -> Int -> [Int]
x <-> y = if x > y then [] else (x:((x+1) <-> y))

(<-->) :: Int -> Int -> [Int]
x <--> y
   | x > y = []
   | x <= y = (x:((x+1) <--> y))

factorial2 :: Int -> Int
factorial2 a = product (1 <-> a)

--Ejercicio 7
ultimo :: [a] -> a
ultimo (x:[]) = x
ultimo (h:c) = ultimo c

sacarUltimo :: [a] -> [a]
sacarUltimo (x:(x2:[])) = (x:[])
sacarUltimo (x:xs) = (x:(sacarUltimo(xs)))

--Ejercicio 8
capicua :: Eq a => [a] -> Bool
capicua a = if ((rev a) == a) then True else False

--Ejercicio 9
flat :: [[a]] -> [a]
flat (x:[]) = x
flat (x:xs) = append x (flat xs)

longLl :: [[a]] -> Int
longLl (x:[]) = long x
longLl (x:xs) = long x + longLl xs


--Ejercicio 10
intercalar :: [[a]] -> [[a]] -> [[a]]
intercalar (a:[]) (x:[]) = [a,x]
intercalar (a:b) (x:y) = append [a,x] (intercalar b y)

aparear :: [Int] -> [Int] -> [Int]
aparear a [] = a
aparear [] b = b
aparear (a:b) (x:y)
    | a <= x = (a: aparear b (x:y)) 
    | a > x = (x: aparear (a:b) y)

--Ejercicio 11
-- decAHex :: Double -> [Char]
-- decAHex x
--     | x == 10 = ['A']
--     | x == 11 = ['B']
--     | x == 12 = ['C']
--     | x == 13 = ['D']
--     | x == 14 = ['E']
--     | x == 15 = ['F']
--     | (x < 1) && (x > 0) = decAHex (x*16)
--     | (x >= 1x < 10 = [show x]
--     | x > 15 = decAHex (x/16)


--Ejercicio 12
perfecto :: Int -> Bool
perfecto x = sumDivisibles x (x-1) == x

sumDivisibles :: Int -> Int -> Int
sumDivisibles x y
    | y == 1 = y
    | divisiblePor x y = y + sumDivisibles x (y-1)
    | otherwise = sumDivisibles x (y-1) 

--Ejercicio 13
posicion :: Eq a => ([[a]],[a]) -> Int
posicion ((x:xs),y)
    | x == y = 1
    | x /= y = 1 + posicion(xs,y)

subcadena :: [Char] -> Int -> Int -> [Char]
subcadena [] z y = []
subcadena x 1 y = tomar y x
subcadena (x:xs) z y = subcadena xs (z-1) y

--Ejercicio 14
-- pendiente

--Ejercicio 15
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = (partes xs) ++ (addAll x (partes xs))

addAll :: a -> [[a]] -> [[a]]
addAll x [] = []
addAll x (h:c) = ((x:h) : (addAll x c))

-- NO CURRICADO 
-- addAll :: a -> ([[a]] -> [[a]])
-- addAll (x,[]) = []
-- addAll (x,(h:c)) = ((x:h) : (addAll (x,c)))

