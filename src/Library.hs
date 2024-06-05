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

type Palos = [PaloDeGolf]

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

palosDisponiblesEnElJuego :: Palos
palosDisponiblesEnElJuego = [putter, madera] ++ map hierros [1..10] 

--Punto 2
golpe :: Jugador -> PaloDeGolf -> Tiro
golpe jugador palo = palo (habilidad jugador)

--Punto 3
type Obstaculo = Tiro -> Tiro
type Condicion = Tiro -> Bool
type Efecto = Tiro -> Tiro

tiroParado :: Tiro
tiroParado = UnTiro 0 0 0

tunelConRampita :: Obstaculo
tunelConRampita tiro = tiroVsObstaculo tiro superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = (precision tiro > 90) && (altura tiro == 0)

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = tiro{velocidad = (doble . velocidad) tiro, precision = 100, altura = 0}

laguna :: Number -> Obstaculo
laguna largo tiro = tiroVsObstaculo tiro superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = (velocidad tiro > 80) && between 1 5 (altura tiro)

efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiro = tiro{altura = altura tiro / largo}

hoyo :: Obstaculo
hoyo tiro = tiroVsObstaculo tiro superaHoyo efectoHoyo

superaHoyo :: Condicion
superaHoyo tiro = between 5 20 (velocidad tiro) && (precision tiro > 95) && (altura tiro == 0)

efectoHoyo :: Efecto
efectoHoyo tiro = tiroParado

tiroVsObstaculo :: Tiro -> Condicion -> Efecto -> Tiro
tiroVsObstaculo tiro condicion efecto 
    | condicion tiro = efecto tiro
    | otherwise = tiroParado 

palosUtiles :: Jugador -> Obstaculo -> Palos
palosUtiles jugador obstaculo = filter(paloSuperaObstaculo jugador obstaculo) palosDisponiblesEnElJuego

paloSuperaObstaculo :: Jugador -> Obstaculo -> PaloDeGolf -> Bool
paloSuperaObstaculo jugador obstaculo palo = obstaculo (golpe jugador palo) /= tiroParado 

tiroSuperaObstaculo :: Tiro -> Obstaculo -> Bool
tiroSuperaObstaculo tiro obstaculo = obstaculo tiro /= tiroParado

cuantosObstaculosConsecutivosSupera :: [Obstaculo] -> Tiro -> Number
cuantosObstaculosConsecutivosSupera [] _ = 0
cuantosObstaculosConsecutivosSupera (x:xs) tiro 
    | tiroSuperaObstaculo tiro x = 1 + cuantosObstaculosConsecutivosSupera xs tiro
    | otherwise = 0

elPaloMasUtil :: Jugador -> [Obstaculo] -> PaloDeGolf
elPaloMasUtil jugador obstaculos = 
    maximoSegun (\palo -> cuantosObstaculosConsecutivosSupera obstaculos (golpe jugador palo)) palosDisponiblesEnElJuego








