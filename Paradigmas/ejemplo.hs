esVacia :: [a] -> Bool
esVacia [] = True
esVacia (_:_) = False

producto 0 n = n
producto m n = n + (producto (m-1) n)

--Def 1
--(<->) :: Int -> Int -> [Int]
--x <-> y = if (x > y) then [] else (x:(x + 1) <-> y)

--Def 2
(<->) :: Int -> Int -> [Int]
x <-> y
 | x > y = []
 | otherwise = (x:(x + 1) <-> y)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

factorial2 :: Int -> Int
factorial2 n = product (1 <-> n)


partes :: [a] -> [[a]]
--
-- partes [] = ?????
-- partes (x:xs) = .... x .... (partes xs) ....
--

--
-- partes []      = [ [] ]       --- ([]:[])
-- partes [9]     =   9     partes []      = [ [], [9] ]
--                            [ [] ]
-- partes [7,9]   =   7     partes [9]     = [ [], [7], [9], [7,9] ]
--                          [ [], [9] ]
-- partes [2,7,9] =   2     partes [7,9]     = [ [], [2], [7], [9], [7,9], [2,7], [2,9], [2,7,9] ]
--                     [ [], [7], [9], [7,9] ]    *   /    *    *     *      /      /       /

partes []     = [[]]
--partes (x:xs) = (partes xs) ++ (addAll x (partes xs))
--partes (x:xs) = yss ++ (addAll x yss) where yss = (partes xs)
partes (x:xs) = let yss = (partes xs) in yss ++ (addAll x yss)

-- addAll :: a -> [[a]] -> [[a]]
-- addAll 9 [[6], [8,3], [], [7,7,8]] -> [[9,6], [9,8,3], [9], [9,7,7,8]]
addAll x [] = []
addAll x (ys:yss) = ((x:ys):addAll x yss)

addAlln :: (a,[[a]]) -> [[a]]
addAlln (x,[]) = []
addAlln (x,(ys:yss)) = ((x:ys):addAlln (x,yss))

-- ==============================================================

verdadero :: Bool
verdadero = True

data Color = Azul Int | Rojo | Verde | Amarillo | Violeta | Rosa | Naranja deriving (Eq, Show)

azul :: Int -> Color
azul n = Azul (2*n)

tonalidad :: Color -> Int
tonalidad (Azul n) = n
tonalidad _ = 1

esPrimario :: Color -> Bool
esPrimario (Azul _) = True
esPrimario Rojo = True
esPrimario Amarillo = True
esPrimario _ = False

data Lista a = ListaVacia | ListaNoVacia a (Lista a) deriving (Eq, Show)

cabeza :: Lista a -> a
cabeza ListaVacia = error "No definida"
cabeza (ListaNoVacia x xs) = x

cola :: Lista a -> Lista a
cola ListaVacia = error "No definida"
cola (ListaNoVacia x xs) = xs

convertirLista :: Lista a -> [a]
convertirLista ListaVacia = []
convertirLista (ListaNoVacia x xs) = (x:convertirLista xs)

data Grupo = O | A | B | AB deriving (Eq, Show)
type Factor = Bool
data TipoDeSangre = TS Grupo Factor deriving (Eq, Show)
type TipoDeSangre2 = (Grupo, Factor)

donaG :: Grupo -> Grupo -> Bool
donaG O x = True
donaG A x = elem x [A,AB]
donaG B x = elem x [B,AB]
donaG AB x = (x == AB)

dona :: TipoDeSangre -> TipoDeSangre -> Bool
dona (TS g1 f1) (TS g2 f2) = (f1 == f2) && (donaG g1 g2)


data Poste = Origen | Destino | Auxiliar deriving Show
type Movimiento = (Poste, Poste)

hanoi :: Int -> [Movimiento]
hanoi n = hanoi2 n Origen Destino Auxiliar

hanoi2 :: Int -> Poste -> Poste -> Poste -> [Movimiento]
hanoi2 n o d a
 | n == 0 = []
 | n > 0  = (hanoi2 (n-1) o a d) ++ (o,d):(hanoi2 (n-1) a d o)


--data ArbBin a = ArbolVacio | ArbolNoVacio a (ArbBin a) (ArbBin a) deriving (Eq, Show)

--data ArbBinRotHoj a = ArbolVacio | Hoja a | ArbolNoVacio (ArbBinRotHoj a) (ArbBinRotHoj a) deriving (Eq, Show)
data ArbBinRotHoj a = Hoja a | ArbolNoVacio (ArbBinRotHoj a) (ArbBinRotHoj a) deriving (Eq, Show)

data ArbBinRotHoj2 a b = Hoja2 a | Arbol2 b (ArbBinRotHoj2 a b) (ArbBinRotHoj2 a b) deriving (Eq, Show)


--arbBinRotHojesVacio :: ArbBinRotHoj a -> Bool
--arbBinRotHojesVacio ArbolVacio = True
--arbBinRotHojesVacio (Hoja a) = False
--arbBinRotHojesVacio (ArbolNoVacio si sd) = False

--arbNVBinRotHoj :: ArbBinRotHoj a -> ArbBinRotHoj a -> ArbBinRotHoj a
--arbNVBinRotHoj si sd
-- | (arbBinRotHojesVacio si) && (arbBinRotHojesVacio sd) = error "Está mal!"
-- | otherwise = ArbolNoVacio si sd


data ArbN a = ArbN a [ArbN a] deriving (Eq, Show)

-- ////// --

data Op1 = Neg | Cuad deriving (Eq, Show)
data Op2 = Suma | Resta | Prod | Div deriving (Eq, Show)
data Expresion = Cte Int | OpUni Op1 Expresion | OpBin Op2 Expresion Expresion deriving (Eq, Show)

eval :: Expresion -> Int
eval (Cte n) = n
eval (OpUni op1 e) = evalop1 op1 (eval e)
eval (OpBin op2 e1 e2) = evalop2 op2 (eval e1) (eval e2)

evalop1 :: Op1 -> Int -> Int
evalop1 Neg n = 0-n
evalop1 Cuad n = n * n

evalop2 :: Op2 -> Int -> Int -> Int
evalop2 Suma n1 n2 = n1 + n2
evalop2 Resta n1 n2 = n1 - n2
evalop2 Prod n1 n2 = n1 * n2
evalop2 Div n1 n2 = div n1 n2


--eval(
--OpBin Prod
--        (OpUni Cuad (Cte 3))
--        (OpBin Suma 
--                (OpBin Resta
--                         (Cte 5)
--                         (Cte 3)
--                )
--                (Cte 7)
--        )
--)

data Simbolo = S1 Int | S2 Op1 | S3 Op2 deriving (Eq, Show)
type Pila = [Int]

evalPostfix :: [Simbolo] -> Int
evalPostfix ss = eval2 ss []

eval2 :: [Simbolo] -> Pila -> Int
eval2 [] (v:[]) = v
eval2 ((S1 x):ss) ps = eval2 ss (x:ps)
eval2 ((S2 op1):ss) (v:ps) = eval2 ss ((evalop1 op1 v):ps)
eval2 ((S3 op2):ss) (v2:(v1:ps)) = eval2 ss ((evalop2 op2 v1 v2):ps)

-- -----------

fnc :: (Int, [a]) -> Int
fnc (x, ys) = x * (length ys) + 1

curryNuestro :: ((a, b) -> c) -> a -> b -> c
--curryNuestro f = (\x -> (\y -> f (x,y)))
--curryNuestro f = (\x y -> f (x,y))
curryNuestro f x y = f (x,y)

uncurryNuestro :: (a -> b -> c) -> (a, b) -> c
uncurryNuestro f (x,y) = f x y

ifNuestro :: Bool -> a -> a -> a
ifNuestro x r1 r2 = if x then r1 else r2

whileNuestro :: (a -> Bool) -> (a -> a) -> a -> a
whileNuestro c f x = if (c x) then whileNuestro c f (f x) else x

--def lengthNuestro(xs): Int
--var r: Int
--  r <- 0
--  while (not(isEmptyList(xs))) do
--    r <- r + 1
--    xs <- tail xs
--  endwhile
--  return r
--end

lengthNuestro :: [a] -> Int
lengthNuestro xs = fst (whileNuestro (\(r,xs) -> not (null xs)) (\(r,xs) -> (r+1,tail xs)) (0,xs))

mapNuestro :: (a -> b) -> [a] -> [b]
mapNuestro f [] = []
mapNuestro f (x:xs) = ((f x):map f xs)

--map h [x1, x2, x3, ..., xn] -> [h x1, h x2, h x3, ..., h xn]
--map even [7,3,4,8,32,5]

mapNuestro2 f xs = foldr (\x ys -> (f x:ys)) [] xs

--Prac3 Ej10
--a)
paraCadaCola :: Int -> Int -> a -> (a -> Int -> a) -> a
paraCadaCola ini fin x f = if ini > fin then x else paraCadaCola (ini+1) fin (f x ini) f  --recursivo de cola

paraCadaPila :: Int -> Int -> a -> (a -> Int -> a) -> a
paraCadaPila ini fin x f = if ini > fin then x else f (paraCadaPila ini (fin-1) x f) fin  --recursivo de pila
--b)
todos :: (a -> Bool) -> [a] -> Bool
todos f xs = paraCadaCola 0 ((length xs)-1) True (\r y -> r && (f (xs!!y)))
--c)
ninguno :: (a -> Bool) -> [a] -> Bool
ninguno f xs = todos (not . f) xs
--d)
igLong :: [[a]] -> Bool
igLong xss = todos (\xs -> (length xs)==(length (last xss))) xss


listInf :: [Int]
listInf = xs where xs = (1:(map (+1) xs))

--xs = (1:(map (+1) xs))
--
--    xs/(1:(map (+1) xs2))
--
--(1:(map (+1) xs2)) = (1:(map (+1) (1:(map (+1) xs2))))
--
--    xs2/(1:(map (+1) xs3))
--
--(1:(map (+1) (1:(map (+1) xs3)))) = (1:(map (+1) (1:(map (+1) (1:(map (+1) xs3))))))
--
--
--

fib :: [Int]
fib = xs where xs = (1:(1:(zipWith (+) xs (tail xs))))

--xs                  1 1 2 3 5   8  13
--tail xs             1 2 3 5 8  13
--------
--zipWith (+)         2 3 5 8 13 21


data Nada a b c = Cons1 b | Cons2 (a c) deriving (Eq, Show)

-- :k (a b) es *
--
-- :k a es (* -> *)
-- :k b es *

type Estado s a = (s -> (s,a))

resultados :: Estado s a -> s -> [a]
resultados f e = (r2:resultados f e2) where (e2,r2) = (f e)

-- =========================
--   Recuperatorio 2018-1
-- =========================
--Ej 1
traspuesta :: [[a]] -> [[a]]
traspuesta ((x:xs):xss) = ((cabezas ((x:xs):xss)):traspuesta (colas ((x:xs):xss))) 
traspuesta ([]:xss) = []

cabezas :: [[a]] -> [a]
--cabezas (xs:xss) = map head (xs:xss)
cabezas [] = []
cabezas (xs:xss) = (head xs):(cabezas xss)

colas :: [[a]] -> [[a]]
--colas (xs:xss) = map tail (xs:xss)
colas [] = []
colas (xs:xss) = (tail xs):(colas xss)

--Ej 2
--xx z = z (xx z)
--ww = xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b])
--
--EAGER
--ww [6,2,7] =
--xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b]) [6,2,7] =
--(\a b -> if (null b) then b else (a (tail b)) ++ [head b]) (xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b])) [6,2,7] =
--  Error por evaluar lo mismo
--
--LAZY
--ww [6,2,7] =
--xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b]) [6,2,7] =
--(\a b -> if (null b) then b else (a (tail b)) ++ [head b]) (xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b])) [6,2,7] =
--if (null [6,2,7]) then [6,2,7] else ((xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b])) (tail b)) ++ [head [6,2,7]] =
--if false then [6,2,7] else ((xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b])) (tail [6,2,7])) ++ [head [6,2,7]] =
--((xx (\a b -> if (null b) then b else (a (tail b)) ++ [head b])) (tail [6,2,7])) ++ [head [6,2,7]] =
-- . . .


--Ej 3
--i)
data ListaConBase a b = MiLista [a] b
--ii)
foldListaConBase :: (a -> c -> c) -> (b -> c) -> (ListaConBase a b) -> c
foldListaConBase f g lb = foldr f (g (getBase lb)) (getLista lb)

getLista :: ListaConBase a b -> [a]
getLista (MiLista xs y) = xs

getBase :: ListaConBase a b -> b
getBase (MiLista xs y) = y
--iii)
type ListaNoVacia a = ListaConBase a a
--iv)
foldrLNV :: (a -> b -> b) -> b -> ListaNoVacia a -> b
foldrLNV f x lnv = foldListaConBase f (\u -> f u x) lnv


--Ej 4
--El tipo de (map . map)
--
--map :: (a -> b) -> [a] -> [b]
--
--(.) :: (b -> c) -> (a -> b) -> a -> c
--
--((.) map) map
--
--(.) map
--  (.) :: (y -> z) -> (x -> y) -> x -> z
--  map :: (a -> b) -> [a] -> [b]
--
--(.) map    [y/(a -> b), z/[a] -> [b]]
--   :: (x -> (a -> b)) -> x -> [a] -> [b]
--
--(.) map :: (x -> a -> b) -> x -> [a] -> [b]
--map :: (c -> d) -> [c] -> [d]
--
--(.) map map    [x/c -> d, a/[c], b/[d]]
--   :: (c -> d) -> [[c]] -> [[d]]