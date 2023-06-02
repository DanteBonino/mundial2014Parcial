module Lib () where

--Datos que brinda el enunciado:
--Los jugadores los definieron como un data
martin = Jugador "Martin" 26 0.0 50 35.0
juan = Jugador "Juancho" 30 0.2 50 40.0
maxi = Jugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = Jugador "Chueco" 20 1.5 80 99.0
lean = Jugador "Hacha" 23 0.01 50 35.0
brian = Jugador "Panadero" 21 5 80 15.0

garcia = Jugador "Sargento" 30 1 80 13.0
messi =  Jugador "Pulga" 26 10 99 43.0
aguero = Jugador "Aguero" 24 5 90 5.0

--Los equipos como tuplas
equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 

--Punto 1:
data Jugador = Jugador{
    nombre        :: String,
    edad          :: Int,
    promedioDeGol :: Float,
    habilidad     :: Int,
    cansancio     :: Float
}
type Equipo = (String, Char, [Jugador])

figuras :: Equipo -> [Jugador]
figuras = (soloFiguras . jugadores)

jugadores :: Equipo -> [Jugador]
jugadores (_,_, jugadoresDelEquipo) = jugadoresDelEquipo

soloFiguras :: [Jugador] -> [Jugador]
soloFiguras = filter (esFigura)

esFigura :: Jugador -> Bool
esFigura unJugador = ((>75) . habilidad) unJugador && ((>0) . promedioDeGol) unJugador

--Punto 2:
tieneFarandulero :: Equipo -> Bool
tieneFarandulero = (algunoEsFarandulero . jugadores)

algunoEsFarandulero :: [Jugador] -> Bool
algunoEsFarandulero = any (esFarandulero . nombre)

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

--Punto 3:
type Grupo = Char
figuritasDificiles :: Grupo -> [Equipo] -> [String]
figuritasDificiles unGrupo = (concatMap (map nombre . filter (esFiguritaDificil)  . jugadores) . equiposDelGrupo unGrupo)


equiposDelGrupo :: Grupo -> [Equipo] -> [Equipo]
equiposDelGrupo unGrupo = filter ((== unGrupo) . grupo)

grupo :: Equipo -> Grupo
grupo (_, unGrupo, _) = unGrupo

esFiguritaDificil :: Jugador -> Bool
esFiguritaDificil unJugador = esFigura unJugador && esJoven unJugador && (not . esFarandulero) unJugador

esFarandulero :: Jugador -> Bool
esFarandulero = (flip elem jugadoresFaranduleros . nombre)

esJoven :: Jugador -> Bool
esJoven = ((<27) . edad)

--Punto 4:
jugarPartido :: Equipo -> Equipo
jugarPartido  = mapearJugadores (map cambiarCansancio)

mapearJugadores ::([Jugador]->[Jugador]) -> Equipo -> Equipo
mapearJugadores f (unNombre, unGrupo, unosJugadores) = (unNombre, unGrupo, f unosJugadores)

cambiarCansancio :: Jugador -> Jugador
cambiarCansancio unJugador
    | esFiguritaDificil unJugador = ponerOtroCansancio 50      unJugador
    | esJoven unJugador           = multiplicarCansancio 1.1   unJugador
    | esFigura unJugador          = aumentarCansancio 10       unJugador
    | otherwise                   = multiplicarCansancio 1.5   unJugador

ponerOtroCansancio :: Float -> Jugador -> Jugador
ponerOtroCansancio nuevoCansancio unJugador = unJugador {cansancio = nuevoCansancio}

modificarCansancio :: (Float -> Float) -> Jugador -> Jugador
modificarCansancio modificacion unJugador = unJugador {cansancio = (modificacion . cansancio) unJugador}

aumentarCansancio :: Float -> Jugador -> Jugador
aumentarCansancio unValor = modificarCansancio (+ unValor)

multiplicarCansancio :: Float -> Jugador -> Jugador
multiplicarCansancio unValor = modificarCansancio (* unValor)




