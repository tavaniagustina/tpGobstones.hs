import Data.List (delete)
----------------
--- punto 01 ---
----------------

data Tablero = Tablero {
    tamaño :: Tamaño,
    cabezal :: Cabezal,
    celdas :: [Celda]
} deriving (Show)

data Celda = Celda {
    coordenada :: Coordenada,
    bolitas :: [Bolita]
} deriving (Show)

data Bolita = Rojo | Azul | Verde | Negro deriving (Show, Eq)

data Direccion = Norte | Este | Sur | Oeste deriving (Show)

type Coordenada = (Int, Int)
type Tamaño =  (Int, Int)
type Cabezal =  Coordenada

tablero = inicializar (3,3)
tablero2x2 = inicializar (2,2)

----------------
--- punto 02 ---
----------------

inicializar :: Tamaño -> Tablero
inicializar tamaño = Tablero tamaño (1,1) (celdasPara tamaño)

celdasPara :: Tamaño -> [Celda]
celdasPara (x, y) = 
    concatMap (zipWith (\c f -> Celda (c,f) []) [1..x] . repeat ) [1..y]

celdasPara' :: Tamaño -> [Celda]
celdasPara' (x, y) = 
    map (flip Celda []) (concatMap (\fila -> zip [1..x] (repeat fila)) [1..y])

celdasPara'' :: Tamaño -> [Celda]
celdasPara'' (columnas, filas) = [Celda (x,y) [] | x <- [1..columnas], y <- [1..filas]]

----------------
--- punto 03 ---
----------------

mover :: Direccion -> Sentencia
mover direccion tablero 
    | puedeMoverse direccion tablero = mapCabezal (moverHacia direccion) tablero
    | otherwise                      = error "El cabezal se cayo del tablero"

moverHacia :: Direccion -> Cabezal -> Cabezal
moverHacia direccion cabezal = sumar (versor direccion) cabezal

versor :: Direccion -> Coordenada
versor Norte = (0, 1)
versor Sur   = (0, -1)
versor Este  = (1, 0)
versor Oeste = (-1, 0)

sumar (a, b) (c, d) = (a + c, b + d)

mapCabezal :: (Cabezal -> Cabezal) -> Tablero -> Tablero
mapCabezal funcion tablero = tablero { cabezal = funcion $ cabezal tablero }

poner :: Bolita -> Sentencia
poner bolita tablero = mapCeldaActual (agregar bolita) tablero

agregar :: Bolita -> Celda -> Celda
agregar bolita celda = mapBolitas (bolita :) celda

mapBolitas :: ([Bolita] -> [Bolita]) -> Celda -> Celda
mapBolitas funcion celda = celda { bolitas = funcion $ bolitas celda }

mapCeldaActual :: (Celda -> Celda) -> Tablero -> Tablero
mapCeldaActual funcion tablero = tablero {
    celdas = mapSegun (esCeldaActual tablero) funcion $ celdas tablero
}

mapSegun :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapSegun condicion funcion lista = map (aplicarSiCumple condicion funcion) lista

aplicarSiCumple :: (a -> Bool) -> (a -> a) -> a -> a
aplicarSiCumple condicion funcion elemento
    | condicion elemento = funcion elemento
    | otherwise          = elemento 

esCeldaActual :: Tablero -> Celda -> Bool
esCeldaActual tablero celda = cabezal tablero == coordenada celda

sacar :: Bolita ->  Sentencia
sacar bolita tablero = mapCeldaActual (quitar bolita) tablero

quitar :: Bolita -> Celda -> Celda
quitar bolita celda 
    | hayBolitaEnCelda bolita celda = mapBolitas (delete bolita) celda
    | otherwise                     = error $ "No hay bolita del color " ++ show bolita ++ " para sacar de la celda actual"

hayBolitaEnCelda :: Bolita -> Celda -> Bool
hayBolitaEnCelda bolita celda = elem bolita (bolitas celda)

----------------
--- punto 04 ---
----------------

type Sentencia = Tablero  -> Tablero

repetir :: Int -> [Sentencia] -> Sentencia
repetir cantidad sentencias tablero = aplicarSentencias (concat (replicate cantidad sentencias)) tablero

aplicarSentencias :: [Sentencia] -> Tablero -> Tablero
aplicarSentencias sentencias tablero = foldl (flip ($)) tablero sentencias

type Condicion = Tablero -> Bool

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Sentencia
alternativa condicion sentenciasTrue sentenciasFalse tablero 
    | condicion tablero = aplicarSentencias sentenciasTrue tablero
    | otherwise         = aplicarSentencias sentenciasFalse tablero

si :: Condicion -> [Sentencia] -> Sentencia
si condicion sentencias = alternativa condicion sentencias []

siNo :: Condicion -> [Sentencia] -> Sentencia
siNo condicion sentencias = alternativa condicion [] sentencias

mientras :: Condicion -> [Sentencia] -> Sentencia
mientras condicion sentencias tablero = si condicion (sentencias ++ [mientras condicion sentencias]) tablero

irAlBorde :: Direccion -> Sentencia
irAlBorde direccion = mientras (puedeMoverse direccion) [mover direccion]

----------------
--- punto 05 ---
----------------

puedeMoverse :: Direccion -> Condicion
puedeMoverse direccion tablero = estaEntre (1, 1) (tamaño tablero) (moverHacia direccion (cabezal tablero))

estaEntre :: Coordenada -> Coordenada -> Coordenada -> Bool
estaEntre (lix, liy) (lsx, lsy) (x, y) = between lix lsx x && between liy lsy y

between :: Int -> Int -> Int -> Bool
between li ls x = li <= x && x <= ls

hayBolita :: Bolita -> Condicion
hayBolita bolita tablero = hayBolitaEnCelda bolita (celdaActual tablero)

celdaActual :: Tablero -> Celda
celdaActual tablero = find (esCeldaActual tablero) (celdas tablero)

find :: (a -> Bool) -> [a] -> a
find condicion = head . filter condicion 

cantidadBolitas :: Bolita -> Tablero -> Int
cantidadBolitas bolita = count (== bolita) . bolitas . celdaActual 

count :: (a -> Bool) -> [a] -> Int
count condicion = length . filter condicion 

----------------
--- punto 06 ---
----------------

programa :: Tablero -> [Sentencia] -> Tablero
programa tablero sentencias = aplicarSentencias sentencias tablero

----------------
--- punto 07 ---
----------------

programa1 = programa tablero [
    poner Rojo,
    mover Norte,
    repetir 10 [
        poner Rojo,
        poner Verde
    ],
    mientras ((< 9) . cantidadBolitas Rojo) [
        poner Verde,
        poner Rojo
    ]
 ]