    -- TP 2

-- 3
data Grupo = A | B | O | AB deriving (Eq, Show)
type Factor = Bool
data TipoDeSangre = TS Grupo Factor deriving (Eq, Show)

donaGrupo :: Grupo -> Grupo -> Bool
donaGrupo O x = True
donaGrupo A x = elem x [A,AB]
donaGrupo B x = elem x [B,AB]
donaGrupo AB x = (x == AB)

dona :: TipoDeSangre -> TipoDeSangre -> Bool
dona (TS g1 f1) (TS g2 f2) = (f1 == f2) && donaGrupo g1 g2 


----------------------------------------------------------------------

-- 4 

data Nat = Cero | Suc Nat deriving (Eq, Ord, Show)

toInt :: Nat -> Int 
toInt Cero = 0
toInt (Suc x) = 1 + (toInt x)

fromInt :: Int -> Nat
fromInt 0 = Cero
fromInt x = Suc (fromInt (x-1))

suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (Suc n) m = Suc (suma n m)

resta :: Nat -> Nat -> Nat
resta Cero _ = Cero
resta n Cero = n
resta (Suc n) (Suc m) = resta n m

productoTrucho :: Nat -> Nat -> Nat
productoTrucho x y = fromInt((toInt x)*(toInt y))

producto :: Nat -> Nat -> Nat
producto Cero _ = Cero
producto (Suc x) y = suma (producto x y) y

----------------------------------------------------------------------------------
--Hanoi, de la Clase

data Poste = Origen | Destino | Auxiliar deriving Show
type Movimiento = (Poste, Poste)

hanoi :: Int -> [Movimiento]
hanoi n = hanoi2 n Origen Destino Auxiliar

hanoi2 :: Int -> Poste -> Poste -> Poste -> [Movimiento]
hanoi2 n o d a
    | n == 0 = []
    | n > 0 = (hanoi2 (n-1) o a d) ++ (o,d):(hanoi2 (n-1) a d o)

-----------------------------------------------------------------------------------
--6
infixr 5 :-:  
data ListOrd a = EmptyList | a :-: (ListOrd a) deriving (Show, Read, Eq, Ord)   

isEmpty :: ListOrd a -> Bool
isEmpty EmptyList = True
isEmpty _ = False

cab :: (ListOrd a) -> a
cab (x:-:xs) = x

cola :: (ListOrd a) -> (ListOrd a)
cola (x:-:xs) = xs

addOrd :: (Ord a) => a -> ListOrd a -> ListOrd a
addOrd x EmptyList = (x:-:EmptyList)
addOrd x (y:-:ys) = if x<y then (x:-:(y:-:ys)) else (y:-:(addOrd x ys))

-----------------------------------------------------------------------------------

--8
--
data Pila a = PilaVacia | Pila a (Pila a) deriving (Show, Eq)

ponerPila :: a -> (Pila a) -> (Pila a)
ponerPila p ps = (Pila p ps)

sacarPila :: (Pila a) -> a
sacarPila (Pila p ps) = p

sacarNPila :: Int -> (Pila a) -> a
sacarNPila 0 (Pila p ps) = p
sacarNPila n (Pila p ps) = sacarNPila(n-1) ps

crearPila :: (Pila a)
crearPila = PilaVacia

esPilaVacia :: (Pila a) -> Bool
esPilaVacia PilaVacia = True
esPilaVacia _ = False

data Cola a = ColaVacia | Cola a [a] deriving (Show, Eq)

crearCola :: (Cola a)
crearCola = ColaVacia

esColaVacia :: (Cola a) -> Bool
esColaVacia ColaVacia = True
esColaVacia _ = False

ponerCola :: a -> (Cola a) -> (Cola a)
ponerCola x (Cola c cs) = (Cola x (c:cs))
ponerCola x ColaVacia = (Cola x [])

sacarCola :: (Cola a) -> a
sacarCola (Cola c cs) = ultimo cs

ultimo :: [a] -> a
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

----------------------------------------------------------
--9

data TreeBin a = EmptyTreeBin | Tree a (TreeBin a) (TreeBin a) deriving (Eq, Show)

esVacio :: TreeBin a -> Bool
esVacio EmptyTreeBin = True
esVacio _ = True

nodo :: TreeBin a -> a
nodo (Tree x hi hd) = x

hijoDer :: TreeBin a -> TreeBin a
hijoDer (Tree x hi hd) = hi

hijoIzq :: TreeBin a -> TreeBin a
hijoIzq (Tree x hi hd) = hd

nroNodos :: TreeBin a -> Int
nroNodos EmptyTreeBin = 0
nroNodos (Tree x hi hd) = 1 + (nroNodos hi) + (nroNodos hd)

altura :: TreeBin a -> Int
altura EmptyTreeBin = 0
altura (Tree x hi hd) = if (altura hi) > (altura hd) then (altura hi)+1 else (altura hd)+1

preorden :: TreeBin a -> [a]
preorden EmptyTreeBin = []
preorden (Tree x hi hd) = (x:(preorden hi)) ++ (preorden hd)

inorden :: TreeBin a -> [a]
inorden EmptyTreeBin = []
inorden (Tree x hi hd) = (inorden hi) ++ (x:(inorden hd))

postorden :: TreeBin a -> [a]
postorden EmptyTreeBin = []
postorden (Tree x hi hd) = (postorden hi) ++ (postorden hd) ++(x:[])

-- Arbol de prueba
-- Tree 1 (Tree 2 (Tree 4 (Tree 8 EmptyTreeBin EmptyTreeBin) EmptyTreeBin) (Tree 6 EmptyTreeBin EmptyTreeBin)) (Tree 3 (Tree 5 (Tree 9 EmptyTreeBin EmptyTreeBin) EmptyTreeBin) (Tree 7 EmptyTreeBin EmptyTreeBin))
-- Fin Arbol de Prueba
--------------------------------------------------------------------------
--10
--data TreeBinRotHoja a = Empty | Hoja a | Tree (TreeBinRotHoja a) (TreeBinRotHoja a) deriving (Show, Eq)


------------------------------------------------------------------------------------------------------------
--ej 11
--
--REIVISAR, SACAR ARBOL VACIO, CASO BASE ES LISTA DE HOJAS VACIA
--

data TreeGen a = EmptyTreeGen | Treegen a [TreeGen a] deriving(Show, Eq)

isEmptyTreeGen :: TreeGen a -> Bool
isEmptyTreeGen EmptyTreeGen = True
isEmptyTreeGen _ = False

nroNodosGen :: TreeGen a -> Int
nroNodosGen EmptyTreeGen = 0
nroNodosGen (Treegen x xs) = 1 + (recorrerTreeGen xs)

recorrerTreeGen :: [TreeGen a] -> Int
recorrerTreeGen [] = 0
recorrerTreeGen (t:ts) = (nroNodosGen t)+ (recorrerTreeGen ts)

alturaGen :: TreeGen a -> Int
alturaGen EmptyTreeGen = 0
alturaGen (Treegen x xs) = 1 + (maxIntinList (alturaGenL xs))

alturaGenL :: [TreeGen a] -> [Int]
alturaGenL [] = []
alturaGenL (t:ts) = ((alturaGen t):(alturaGenL ts))

maxIntinList :: [Int] -> Int
maxIntinList [] = 0
maxIntinList (x:xs) = if x > maxIntinList(xs) then x else (maxIntinList xs)

preordenGen :: TreeGen a -> [a]
preordenGen EmptyTreeGen = []
preordenGen (Treegen x xs) = (x:(preordenGenL xs))

preordenGenL :: [TreeGen a] -> [a]
preordenGenL [] = []
preordenGenL (t:ts) = (preordenGen t) ++ (preordenGenL ts)

--inordenGen :: TreeGen a -> [a]
--inordenGen EmptyTreeGen = []
--inordenGen (Treegen x xs) = (x:(inordenGenL xs))
--
--inordenGenL :: [TreeGen a] -> [a]
--inordenGenL [] = []

postordenGen :: TreeGen a -> [a]
postordenGen EmptyTreeGen = []
postordenGen (Treegen x xs) = (postordenGenL xs)++ (x:[])

postordenGenL :: [TreeGen a] -> [a]
postordenGenL [] = []
postordenGenL (t:ts) = (postordenGenL ts) ++ (postordenGen t)

--(Treegen 1 [
--    Treegen 2 [], 
--    Treegen 2 [
--        Treegen 3 [],
--        Treegen 3 [
--           Treegen 4 [],
--           Treegen 4 [],
--           Treegen 4 []
--        ],
--        Treegen 3 [],
--        Treegen 3 []
--        ], 
--    Treegen 2 [
--        Treegen 3 [],
--        Treegen 3 [
--           Treegen 4 [],
--           Treegen 4 [
--               Treegen 5 [],
--               Treegen 5 [],
--               Treegen 5 []
--           ],
--           Treegen 4 []
--        ],
--        Treegen 3 []
--        ],
--    Treegen 2 []     
--    ])

-------------------------------------------------------------------------------------------------------------------

--ej 12

data Conjunto a = ConVacio | Con [a] deriving (Show, Eq)

esVacioCon :: Conjunto a -> Bool
esVacioCon ConVacio = True
esVacioCon _ = False

pertenece :: Eq a => Conjunto a -> a -> Bool
pertenece ConVacio _ = False
pertenece (Con cs) c = (elem c cs)

infixr 5 >+<
(>+<) ::  Eq a => Conjunto a -> Conjunto a -> Conjunto a
ConVacio >+< _ = ConVacio
(Con (c:cs)) >+< (Con zs) = if (pertenece (Con zs) c) then (agregar c ((Con cs) >+< (Con zs))) else ((Con cs) >+< (Con zs))

infixr >-<
(>-<) :: Conjunto a -> Conjunto a -> Conjunto a
(>-<) ConVacio ConVacio = ConVacio
(>-<) (Con cs) (Con zs) = (Con (cs ++ zs))

agregar :: a -> Conjunto a -> Conjunto a
agregar x ConVacio = (Con (x:[]))
agregar x (Con xs) = (Con (x:xs))

------------------------------------------------------------------
--ej 21 Expresion

data OpUni = Neg | Cuad deriving (Eq, Show)
data OpBin = Sum | Rest | Mult | Div deriving (Eq, Show)
data Expresion = Cte Int | Op1 OpUni Expresion | Op2 OpBin Expresion Expresion deriving (Eq, Show)

eval :: Expresion -> Int
eval (Cte x) = x
eval (Op1 p e) = evalop1 p (eval e)
eval (Op2 p e i) = evalop2 p (eval e) (eval i)  

evalop1 :: OpUni -> Int -> Int
evalop1 Cuad n = n*n
evalop1 Neg n = 0-n

evalop2 :: OpBin -> Int -> Int -> Int
evalop2 Sum n m = n+m
evalop2 Rest n m = n-m
evalop2 Mult n m = n*m
evalop2 Div n m = div n m

---------

-- Notacion Polaca Inversa

data Simbolo = Cons Int | Opb OpBin | Opu OpUni deriving (Show, Eq)

evalNPI :: [Simbolo] -> Int
evalNPI [] = 0
evalNPI ss = eval2 ss []

eval2 :: [Simbolo] -> [Int] -> Int
eval2 [] (v:[]) = v
eval2 ((Cons x):ss) ps = eval2 ss (x:ps)
eval2 ((Opu op):ss) (v:ps) = eval2 ss ((evalop1 op v):ps)
eval2 ((Opb op):ss) (v2:(v1:ps)) = eval2 ss ((evalop2 op v1 v2):ps)

---ejemplo
-- evalNPI [Cons 2, Cons 3, Opb Sum, Opu Neg, Cons 4, Opb Rest]
-------------------------------------------------------------------

