traspuesta :: [[a]] -> [[a]]
traspuesta [] = []
traspuesta (x:xs) = [(cabeza x xs)] ++ traspuesta (cola (x:xs))
 
cabeza :: [a] -> [[a]] -> [a]
cabeza [] x = []
cabeza (x:xs) [] = [x]
cabeza (x:xs) (y:ys) = (x:(cabeza y ys))

cola :: [[a]] -> [[a]]
cola [] = []
cola ((x:xs):xss) = (xs:(cola xss))

