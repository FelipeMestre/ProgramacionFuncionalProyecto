module AuxFuncScoreHijara
  where
import HijaraTypes
import Conversion
import Data.Matrix (Matrix, getElem)
--Recorre la lista de todas las secciones, si hay una que tiene todas las fichas como las del jugador cuenta 20 puntos
contarDeAVeinteJugador :: Casilla -> HijaraGame -> Int
contarDeAVeinteJugador jugador (NewHijara matrix) = contarVeintePuntos seccionesConCuatroFichas
                        where
                              secciones = concat (hLines matrix)
                              seccionesConCuatroFichas = map (checkearFichasIguales jugador) secciones

checkearFichasIguales :: Casilla -> Matrix Casilla -> Bool
checkearFichasIguales casillaJugador matriz = getElem 1 1 matriz == casillaJugador &&
                                              getElem 1 2 matriz == casillaJugador &&
                                              getElem 2 1 matriz == casillaJugador &&
                                              getElem 2 2 matriz == casillaJugador

contarVeintePuntos :: [Bool] -> Int
contarVeintePuntos [] = 0
contarVeintePuntos (x:xs) = if x then 20 + contarVeintePuntos xs else 0 + contarVeintePuntos xs   

--Cuenta los puntos de a diez de un jugador en concreto, para eso se tienen todas las lineas del tablero y el caracter del jugador
contarDeADiezJugador :: Char -> HijaraGame -> Int
contarDeADiezJugador jugador (NewHijara matrix) = contarDiezPuntos (map (piezasEnElMismoNumero (jugador)) horizontalesTablero) +
                                          contarDiezPuntos (map (piezasEnElMismoNumero (jugador)) verticalesTablero) +
                                          contarDiezPuntos [(piezasEnElMismoNumero jugador (diagonalesTableroDerIzq !! 0) )]  +
                                          contarDiezPuntos [(piezasEnElMismoNumero jugador (diagonalesTableroDerIzq !! 1) )]  +
                                          contarDiezPuntos [(piezasEnElMismoNumero jugador (diagonalesTableroDerIzq !! 2) )]  +
                                          contarDiezPuntos [(piezasEnElMismoNumero jugador (diagonalesTableroIzqDer !! 0) )] +
                                          contarDiezPuntos [(piezasEnElMismoNumero jugador (diagonalesTableroIzqDer !! 1) )] +
                                          contarDiezPuntos [(piezasEnElMismoNumero jugador (diagonalesTableroIzqDer !! 2) )]

                                          where
                                             horizontalesTablero = dividirDeAOcho (matrixToString matrix)
                                             verticalesTablero = verticalesDelTablero (NewHijara matrix)
                                             diagonalesTableroIzqDer = diagonalesDelTablero (NewHijara matrix) 1
                                             diagonalesTableroDerIzq = diagonalesDelTablero (NewHijara matrix) 4

contarDiezPuntos :: [Bool] -> Int
contarDiezPuntos [] = 0
contarDiezPuntos (x:xs) = if x then 10 else 0 + contarDiezPuntos xs                  

--Chequea que todos los caracteres alineados de las secciones sean del jugador dado, 8 chars 
piezasEnElMismoNumero :: Char -> String -> Bool
piezasEnElMismoNumero player cadena = all (player ==) pares || all (player == ) impares
                                    where
                                      pares = [cadena !! x | x <- [0,2,4,6]]
                                      impares = [cadena !! x | x <- [1,3,5,7]]

--Se usa para las diagonales pequeñas porque solo tienen 4 char
piezasEnElMismoNumeroDiagonalPequeña :: String -> Char -> Bool 
piezasEnElMismoNumeroDiagonalPequeña cadena player = all (player ==) cadena



--Cuenta los casos donde el jugador colocó fichas en las 1234 de secciones alineadas
contarDeAQuinceJugador :: Casilla -> HijaraGame -> Int 
contarDeAQuinceJugador casillaJugador (NewHijara matrix) = puntosDiagonales + puntosVerticales + puntosHorizontales
                                               where
                                                        diagonalUno = [getElem x y matrix | (x,y) <- zip [1..4] (reverse [1..4])]
                                                        diagonalDos = [getElem x x matrix | x <- [1..4]]
                                                        diagonales = diagonalUno : [diagonalDos] 
                                                        horizontales = hLines matrix
                                                        verticales = vLines matrix
                                                        puntosDiagonales = contarQuincePuntos (map (esUnoDosTresCuatro casillaJugador) diagonales) 
                                                        puntosHorizontales = contarQuincePuntos (map (esUnoDosTresCuatro casillaJugador) horizontales)
                                                        puntosVerticales = contarQuincePuntos (map (esUnoDosTresCuatro casillaJugador) verticales)

--Cuenta si las casillas en las que tiene ficha el jugador son uno dos tres y cuatro, tambien lo hace para la combinacion a reves.
esUnoDosTresCuatro :: Casilla -> [Matrix Casilla] -> Bool
esUnoDosTresCuatro casilla lista = getElem 1 1 (lista !! 0) == casilla && getElem 1 2 (lista !! 1) == casilla &&
                                   getElem 2 1 (lista !! 2) == casilla && getElem 2 2 (lista !! 3) == casilla ||
                                   getElem 1 1 (alReves !! 0) == casilla && getElem 1 2 (lista !! 1) == casilla &&
                                   getElem 2 1 (alReves !! 2) == casilla && getElem 2 2 (lista !! 3) == casilla       
                         where
                               alReves = reverse lista 

--Cuenta segun una lista de booleanos, si hay un true es que se consisguio 15 puntos y se suman
contarQuincePuntos :: [Bool] -> Int
contarQuincePuntos [] = 0
contarQuincePuntos (x:xs) = if x then 15 + contarQuincePuntos xs else 0 + contarQuincePuntos xs


--Devuelve las diagonales en las que se pueden alinear 4 fichas del tablero
--Una lista con la diagonal grande primero y las siguientes despues
--Comienzo es la fila superior donde empiezaz la diagonal
diagonalesDelTablero :: HijaraGame -> Int -> [String]
diagonalesDelTablero (NewHijara matrix) comienzo 
                                                |comienzo == 1 = [diagonalGrandeMatriz diagonales 1 1 2 2] ++ 
                                                                [diagonalPequeñaDeMatrices diagonales 2 1] ++
                                                                [diagonalPequeñaDeMatrices diagonales 1 2]
                                                |comienzo == 4 = [diagonalGrandeMatriz diagonales 1 2 2 1] ++ 
                                                                [diagonalPequeñaDeMatrices diagonales 1 1] ++
                                                                [diagonalPequeñaDeMatrices diagonales 2 2]
                                                |otherwise = error("Introduzca un indice de diagonal válido")
                          where
                            diagonales = if comienzo == 1 then [getElem x x matrix | x <- [1..4]] else if comienzo == 4 
                              then [getElem x y matrix | (x,y) <- zip [1..4] (reverse [1..4])] else error ("Ingrese un comienzo valido")

--Devuelve un string con las casillas de una lista de matrices que esten en la misma posicion en cada una.
--Sirve para diagonales pequeñas porque se toma un solo elemento por matriz
diagonalPequeñaDeMatrices :: [Matrix Casilla] -> Int -> Int -> String
diagonalPequeñaDeMatrices [] fila columna = []
diagonalPequeñaDeMatrices (x:xs) fila columna = show (getElem fila columna x) ++ diagonalPequeñaDeMatrices xs fila columna

--Devuelve un string con las casillas de una lista de matrices que esten en la misma posicion en cada una.
--Sirve para diagonales grandes porque se toma dos elementos por matriz
diagonalGrandeMatriz :: [Matrix Casilla] -> Int -> Int -> Int -> Int -> String
diagonalGrandeMatriz [] indice1 indice12 indice2 indice21 = []
diagonalGrandeMatriz (x:xs) indice1 indice12 indice2 indice21 = show (getElem indice1 indice12 x) ++ show (getElem indice2 indice21 x) 
                                                                  ++ diagonalGrandeMatriz xs indice1 indice12 indice2 indice21

verticalesDelTablero :: HijaraGame -> [String] --Devuelve una lista con las columnas divididas de a ocho
verticalesDelTablero (NewHijara matrix) = dividirDeAOcho (foldr1 (++) columnasEnString)
                          where
                            verticalesDeTablero = vLines matrix --Todas las verticales del tablero
                            verticalesDeMatriz = [(map vLines x) | x <- verticalesDeTablero] --Hace una lista con las verticales de cada submatriz V
                            --Recorre el tablero en columnas como si los cuadrantes no existieran
                            primerasColumnas = [(((verticalesDeMatriz !! y) !! x) !! z) | y <- [0..3], z <-[0..1], x <- [0..3]] 
                            columnasEnString = map show (pasarALista primerasColumnas)

-- Divide en ocho un string, se usa para obtener lista de columnas y filas en string
dividirDeAOcho :: String -> [String]
dividirDeAOcho [] = []
dividirDeAOcho string = cabeza ++ dividirDeAOcho cola
                  where
                    cabeza = [take 8 string]
                    cola = drop 8 string