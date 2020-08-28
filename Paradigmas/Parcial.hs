--Ejercicio 1.i
paresAux :: Int -> Int -> [(Int,Int)]
paresAux x y = let 
                    j = x + 1
                    i = y - 1 in if (j <= i) then ((j,i):(paresAux j i)) else []

pares :: Int -> [(Int,Int)]
pares x = (paresAux 0 x)

pares_monad :: Int -> [(Int, Int)]
pares_monad x = [1..x] >>= \i -> [1..x] >>= \j -> if((i <= j) && (i+j == x)) 
                                                        then return (i,j) 
                                                        else []


--Ejercicio 3.i
data ArbolNRot a b = NodoNRot a [(b,ArbolNRot a b)] deriving (Show)

--Ejercicio 3.ii
foldrANR :: ((b,ArbolNRot a b) -> c -> c) -> (a -> c) -> (ArbolNRot a b) -> c
foldrANR f g anr = foldr f (g (getNodo anr)) (getLista anr)

getLista :: ArbolNRot a b -> [(b, ArbolNRot a b)]
getLista (NodoNRot a xs) = xs

getNodo :: ArbolNRot a b -> a
getNodo (NodoNRot a xs) = a

--Ejercicio 3.iii
--Se utiliza un foldr dentro para sumar el subarbol
--Retorna una lista donde cada elemento es la sumatoria de los rótulos del sub arbol
rotulosRamas :: ArbolNRot a Int -> [Int]
rotulosRamas arbol = foldrANR (\(x,y) z -> (x + (foldrANR (\(x,y) z -> x + z) (\u -> 0) y):z)) (\u -> []) (arbol)