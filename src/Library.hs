module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--Punto 1
type PaloDeGolf = Habilidad -> Tiro

putter :: PaloDeGolf
putter habilidad = UnTiro{velocidad = 10, precision = (doble . precisionJugador) habilidad , altura = 0}

madera :: PaloDeGolf
madera habilidad = UnTiro{velocidad = 100, precision = (mitad . precisionJugador) habilidad , altura = 5}

mitad :: Number -> Number
mitad numero = numero/2 

hierros :: Number -> PaloDeGolf
hierros n habilidad = 
    UnTiro{velocidad = velociadadHierros n habilidad, precision = precisionHierros n habilidad , altura = alturaHierros n}

velociadadHierros :: Number -> Habilidad -> Number
velociadadHierros n habilidad = fuerzaJugador habilidad * n

precisionHierros :: Number -> Habilidad -> Number
precisionHierros n habilidad = precisionJugador habilidad / n

alturaHierros :: Number -> Number
alturaHierros 0 = 0
alturaHierros 1 = 0
alturaHierros 2 = 0
alturaHierros n = n - 3

data Palos = Putter | Madera | Hierros deriving(Show, Eq)