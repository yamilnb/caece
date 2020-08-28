-- Definición de tipo de dato Color
-- Variante: Azul le metemos tonalidad, hay tantas tonalidades de Azul como colores que se dará la tonalidad por un entero
-- data Id_Dato = Constructor 1 | Constructor 2 | .. | Constructor N
-- Rojo, Verde, Amarillo, etc son CONSTANTES CONSTRUCTORAS
-- Azul FUNCION CONSTRUCTORA

data Color = Azul Int | Rojo | Verde | Amarillo | Violeta | Rosa | Naranja deriving (Eq, Show)

tonalidad :: Color -> Int
tonalidad (Azul n) = n
tonalidad _ = 1

-- Tipo custom Lista
data Lista a = ListaVacia | ListaNoVacia a (Lista a) deriving (Eq, Show)

cabeza :: Lista a -> a
cabeza ListaVacia = error "No definida"
cabeza (ListaNoVacia x xs) = x

cola :: Lista a -> Lista a
cola ListaVacia = error "No definida"
cola (ListaNoVacia x xs) = xs

--definición de Lista llamando a cabeza
-- cabeza (ListaNoVacia 1 (ListaNoVacia 2 ListaNoVacia 3))

--Tipo de sangre
data Grupo = O | A | B | AB deriving (Eq, Show)
type Factor = Bool
data TipoSangre = TS Grupo Factor deriving (Eq, Show)
--otra forma: type TipoSangre = (Grupo, Factor)

donaG :: Grupo -> Grupo -> Bool
donaG O x = True
donaG A x = elem x [A,AB]

dona :: TipoSangre -> TipoSangre -> Bool
dona (TS g1 f1) (TS g2 f2) = (f1 == f2) && (donaG g1 g2)

