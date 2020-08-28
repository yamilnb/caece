--Ej 3
data Grupo = O | A | B | AB deriving (Eq, Show)

type Factor = Bool

data TipoDeSangre = TS Grupo Factor deriving    (Eq, Show)

donaGrupo :: Grupo -> Grupo -> Bool
donaGrupo O _  = True
donaGrupo _ O = True
donaGrupo A x = elem x [A,AB]
donaGrupo B x = elem x [B,AB]

puedeDonarA :: TipoDeSangre -> TipoDeSangre -> Bool
puedeDonarA (TS g1 f1) (TS g2 f2) = if (f1) then (donaGrupo g1 g2) && (f1 == f2 ) else (donaGrupo g1 g2)

-- Ej 4
data Nat = Cero | Suc Nat deriving (Eq, Ord, Show)

suma :: Nat -> Nat -> Nat
suma Cero x = Cero
suma (Suc n) m = Suc (suma n m)

resta :: Nat -> Nat -> Nat
resta x Cero = x
resta (Suc m) (Suc n) = resta m n

producto :: Nat -> Nat -> Nat
producto Cero x = Cero
producto (Suc x) y = suma (producto x y) y

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt (x)

intToNat :: Int -> Nat
intToNat 0 = Cero
intToNat 1 = Suc Cero
intToNat x = Suc (intToNat (x-1))

--Ej 6
data ListOrd a = EmptyList | Lista a (ListOrd a) deriving (Eq, Ord, Show)

headListOrd :: ListOrd a -> a
headListOrd EmptyList = error "Not defined"
headListOrd (Lista x xs) = x

tailListOrd :: ListOrd a -> ListOrd a
tailListOrd (Lista x xs) = xs

addToListOrd :: Ord a => a -> ListOrd a -> ListOrd a
addToListOrd a EmptyList = Lista a (EmptyList)
addToListOrd a (Lista x xs) = if (a <= x) then Lista a (Lista x xs) else Lista x (addToListOrd a xs)

--Ej 8
data Pila a = EmptyPila | ListaPila a (Pila a) deriving (Eq,Ord,Show)

crearPila :: Ord a => a -> Pila a
crearPila x = ListaPila x EmptyPila

esVaciaPila :: Pila a -> Bool
esVaciaPila EmptyPila = True
esVaciaPila (ListaPila x xs) = False

headPila :: Pila a -> a
headPila EmptyPila = error "Not defined"
headPila (ListaPila x xs) = x

pushPila :: Ord a => a -> Pila a -> Pila a
pushPila a EmptyPila = crearPila a
pushPila a (ListaPila x xs) = ListaPila x (pushPila a xs)

popPila :: Ord a => Pila a -> a
popPila (ListaPila x EmptyPila) = x
popPila (ListaPila x xs) = popPila xs

--Ej 9
data ArbBin a = EmptyTreeBin | TreeBin a (ArbBin a) (ArbBin a) deriving (Eq, Show)

nroNodosBin :: ArbBin a -> Int
nroNodosBin EmptyTreeBin = 0
nroNodosBin (TreeBin a b c) = 1 + nroNodosBin b + nroNodosBin c

alturaTreeBin :: ArbBin a -> Int
alturaTreeBin EmptyTreeBin = 0
alturaTreeBin (TreeBin a b c) = 
    let alturaIzq = alturaTreeBin b
        alturaDer = alturaTreeBin c
    in
        if (alturaIzq >= alturaDer) then 1 + alturaIzq else 1 + alturaDer

preorden :: ArbBin a -> [a]
preorden EmptyTreeBin = []
preorden (TreeBin a b c) = (a:(preorden b)) ++ (preorden c)

postorden :: ArbBin a -> [a]
postorden EmptyTreeBin = []
postorden (TreeBin a b c) = (postorden b) ++ (postorden c) ++ [a]

inorden :: ArbBin a -> [a]
inorden EmptyTreeBin = []
inorden (TreeBin a b c) = (inorden b) ++ [a] ++ (inorden c)

igEstrucArbol :: ArbBin a -> ArbBin a -> Bool
igEstrucArbol EmptyTreeBin EmptyTreeBin = True
igEstrucArbol EmptyTreeBin (TreeBin x xr xl) = False
igEstrucArbol (TreeBin x xr xl) EmptyTreeBin = False
igEstrucArbol (TreeBin x xr xl) (TreeBin y yr yl) = (igEstrucArbol xr yr) && (igEstrucArbol xl yl)

--Ej 11
data ArbGen a = EmptyTreeGen | TreeGen a [ArbGen a] deriving (Eq, Show)

nroNodosGen :: ArbGen a -> Int
nroNodosGen EmptyTreeGen = 0
nroNodosGen (TreeGen a x) = 1 + (nroNodosGenAux x)

nroNodosGenAux :: [ArbGen a] -> Int
nroNodosGenAux [] = 0
nroNodosGenAux (x:xs) = (nroNodosGen x) + (nroNodosGenAux xs)

alturaTreeGen :: ArbGen a -> Int
alturaTreeGen EmptyTreeGen = 0
alturaTreeGen (TreeGen a x) = 1 + alturaTreeGenAux x

alturaTreeGenAux :: [ArbGen a] -> Int
alturaTreeGenAux [] = 0
alturaTreeGenAux (x:xs) =
    let alturaHead = alturaTreeGen x
        alturaTail = alturaTreeGenAux xs
    in
        max alturaHead alturaTail

preordenGen :: ArbGen a -> [a]
preordenGen EmptyTreeGen = []
preordenGen (TreeGen x [EmptyTreeGen]) = [x]
preordenGen (TreeGen x []) = [x]
preordenGen (TreeGen a (x:xs)) = [a] ++ (preordenGen x) ++ (preordenGenAux xs)

preordenGenAux :: [ArbGen a] -> [a]
preordenGenAux [] = []
preordenGenAux (x:xs)= (preordenGen x) ++ (preordenGenAux xs)

postordenGen :: ArbGen a -> [a]
postordenGen EmptyTreeGen = []
postordenGen (TreeGen x [EmptyTreeGen]) = [x]
postordenGen (TreeGen x []) = [x]
postordenGen (TreeGen a (x:xs)) = (postordenGen x) ++ (postordenGenAux xs) ++ [a]

postordenGenAux :: [ArbGen a] -> [a]
postordenGenAux [] = []
postordenGenAux (x:xs)= (postordenGen x) ++ (postordenGenAux xs)

inordenGen :: ArbGen a -> [a]
inordenGen EmptyTreeGen = []
inordenGen (TreeGen x [EmptyTreeGen]) = [x]
inordenGen (TreeGen x []) = [x]
inordenGen (TreeGen a (x:xs)) = (inordenGen x) ++ [a] ++ (inordenGenAux xs) 

inordenGenAux :: [ArbGen a] -> [a]
inordenGenAux [] = []
inordenGenAux (x:xs)= (inordenGen x) ++ (inordenGenAux xs)

-- Ejemplo de Arbol
-- (TreeGen 1 [
--    TreeGen 2 [], 
--    TreeGen 3 [
--        TreeGen 31 [],
--        TreeGen 32 [
--           TreeGen 321 [],
--           TreeGen 322 [],
--           TreeGen 323 []
--        ],
--        TreeGen 33 [],
--        TreeGen 34 []
--        ], 
--    TreeGen 4 [
--        TreeGen 41 [],
--        TreeGen 42 [
--           TreeGen 421 [],
--           TreeGen 422 [
--               TreeGen 4221 [],
--               TreeGen 4222 [],
--               TreeGen 4223 []
--           ],
--           TreeGen 423 []
--        ],
--        TreeGen 43 []
--        ],
--    TreeGen 5 []     
--    ])

--Ej 12
data Conjunto a = Conj [a] deriving (Eq, Show)

conjuntoVacio :: Conjunto a -> Bool
conjuntoVacio (Conj []) = True
conjuntoVacio (Conj x) = False

union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union (Conj []) (Conj y) = Conj y
union (Conj y) (Conj []) = Conj y
union (Conj (x:xs)) (Conj y) = if (elem x y) then union (Conj xs) (Conj y) else union (Conj xs) (Conj (x:y))

interseccion :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
interseccion (Conj []) (Conj y) = (Conj [])
interseccion (Conj y) (Conj []) = (Conj [])
interseccion (Conj (x:xs)) (Conj y) = if elem x y then union (Conj [x]) (interseccion (Conj xs) (Conj y)) else interseccion (Conj xs) (Conj y)

data Matriz a = Matrix [[a]] deriving (Eq, Show)

sumaMatriz :: Matriz a -> Matriz a -> Matriz a
sumaMatriz (Matrix x) (Matrix y)= (Matrix (sumaMatrizAux x y))

sumaMatrizAux :: [[a]] -> [[a]] -> [[a]]
sumaMatrizAux (x:xs) (y:ys) = ((sumaMatrizFila (x y)):(sumaMatrizAux(xs ys))

sumaMatrizFila :: [a] -> [a] -> [a]
sumaMatrizFila (x:xs) (y:ys) = (x+y:(sumaMatrizFila(xs ys)))