fn :: [a] -> (a -> a) -> [a]
fn [] f = []
fn (x:xs) f = ((f x): (fn xs f))

--Ej 1
esCerrada :: Eq a => [a] -> ([a] -> [a]) -> Bool
esCerrada a f = if (includeList a (f a))  then True else False

includeList :: Eq a => [a] -> [a] -> Bool
includeList a [] = True
includeList a (x:xs) = if (elem x a) then (includeList a xs) else False

--Ej 2
maxf :: (Num a, Ord a) => [a] -> (a -> a) -> a
maxf (x:[]) f= f x
maxf (x:xs) f = max (f x) (maxf xs f)

--Revisar
minf :: (Num a, Ord a) => [a] -> (a -> a) -> a
minf x f = negate (maxf x f)

--Ej 4
genList :: a -> Int -> (a -> a) -> [a]
genList a 0 f = []
genList a x f= (a:(genList (f a) (x-1) f))

--Ej 5
filter :: [a] -> (a -> Bool) -> [a]
filter [] f = []
filter (x:xs) f = if (f x) then (x:(Main.filter xs f)) else (Main.filter xs f)

--Ej 6
o :: (b -> c) -> (a -> b) -> a -> c
o f g x = f (g x)

--Ej 6 ii ?

--Ej 7
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

--Ej 8
sumaCurry :: Int -> Int -> Int
sumaCurry a b = a + b

divisionCurry :: Fractional a => a -> a -> a
divisionCurry a b = a / b

sumaUncurry :: (Int,Int) -> Int
sumaUncurry (a,b) = a + b

divisionUncurry :: Fractional a => (a, a) -> a
divisionUncurry (a,b) = a / b

sucesor :: Int -> Int
sucesor a = sumaCurry a 1

predecesor :: Int -> Int
predecesor a = sumaCurry a (-1)

mitad :: Fractional a => a -> a
mitad a = divisionCurry a 0.5

dosVeces :: (a -> a) -> a -> a
dosVeces f x = f (f x)

cuatroVeces :: (a -> a) -> a -> a
cuatroVeces f x =  o (dosVeces f) (dosVeces f) x

--Ej 9
separar :: ((a -> Bool),[a]) -> ([a],[a])
separar (f,x) = foldr (\head (xs,ys) -> if (f head) then ((head:xs),ys) else (xs,(head:ys))) ([],[]) x

separarCurry :: (a -> Bool) -> [a] -> ([a],[a])
separarCurry f x = ((Main.filter x f), (Main.filter x (o (not) (f))))

mayoria :: Int -> [[Int]] -> [[Int]]
mayoria a xs = foldr(\x acum -> if ((length (Main.filter x (\y -> y > a))) > a)  then (x:acum) else acum) ([]:[]) xs 

--Ej 10
paraCada :: Int -> Int -> d -> (d -> Int -> d) -> d
paraCada s e d f = if (s > e) then d else (f (paraCada (s+1) e d f) s)