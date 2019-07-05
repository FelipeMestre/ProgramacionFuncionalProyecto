{- Hijara ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2019 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Por Leonardo Val.
-}
module Hijara where

import Data.Maybe (fromJust, listToMaybe)
import Data.Matrix (Matrix, getCol, getRow, ncols, nrows, toList, toLists, fromLists, fromList, submatrix, getElem, setElem)
import Data.Vector (toList)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
import Data.Char
import System.Random

{- Es posible que el paquete System.Random no esté disponible si se instaló el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install random

La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar 
disponible junto con el `ghci`.

-}

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

data HijaraPlayer = BluePlayer | YellowPlayer deriving (Eq, Show, Enum)
data HijaraGame = NewHijara (Matrix (Matrix Casilla))
data HijaraAction = NewAction Int Int Int deriving (Eq, Show)

data Casilla = Blue | Yellow | Empty deriving (Eq)
instance Show Casilla where
  show (Yellow) = "y"
  show (Blue) = "b"
  show (Empty) = "x"

--Crea un tablero de 4 X 4 secciones donde cada seccion tiene una casilla empty
beginning :: HijaraGame
beginning = NewHijara $ fromList 4 4 [fromList 2 2 [Empty | x <- [1..4]] | _ <- [1..16]]


--Devuelve una lista con las posibles jugadas que se pueden realizar según un estado de juego
actions :: HijaraGame -> [(HijaraPlayer, [HijaraAction])]
actions (NewHijara higa) = [(actPlayer, listaMovimientos), (noActPlayer, [])]
                        where
                          actPlayer = activePlayer (NewHijara higa)
                          noActPlayer = if (activePlayer (NewHijara higa) == BluePlayer) then YellowPlayer else BluePlayer
                          filas = hLines higa
                          -- En la siguiente linea "x" va a iterar con cada fila, "y" solo indica el numero de la fila y "z" itera las columnas el numero de la columna
                          listaMovimientos = [ NewAction y z ((fromJust (elemIndex Empty (Data.Matrix.toList (x !! (z-1))))) + 1) | (x,y) <- zip filas [1..4], z <- [1..4], Empty `elem` (Data.Matrix.toList (x !! (z-1))) ]


--Dado un tablero y una jugada la realiza de ser posible
next :: HijaraGame -> (HijaraPlayer, HijaraAction) -> HijaraGame
next (NewHijara hija) (player, NewAction fila columna valor)
  | player /= (activePlayer (NewHijara hija)) =  (error "El jugador no es el jugador activo")
  | not ((NewAction fila columna valor) `elem` posiblePlayerActions) = error "No se puede efectuar la accion ingresada"
  | otherwise = NewHijara (setElem nuevaSeccion (fila, columna) hija   )
  where
    posiblePlayerActions = if (fst ((actions (NewHijara hija) !! 0)) == player)
      then snd ((actions (NewHijara hija) !! 0))
      else snd ((actions (NewHijara hija) !! 1))
    casillaDelJugador = if (player == YellowPlayer) then Yellow else Blue
    seccion = getElem fila columna hija --Saca la seccion del tablero
    casillaUno = (1,1) 
    casillaDos = (1,2)
    casillaTres = (2,1)
    casillaCuatro = (2,2) --Se fija la coordenada de la casilla en la subMatriz
    casilla = if valor == 1 then casillaUno else if valor == 2 then casillaDos else if valor == 3 then casillaTres else casillaCuatro
    nuevaSeccion = setElem  casillaDelJugador casilla seccion


result :: HijaraGame -> [(HijaraPlayer, Int)]
result b 
      |isFinished b = if scoreP1 > scoreP2 then [(p1,1),(p2,-1)] else if scoreP1 < scoreP2 then [(p1,-1),(p2,1)] else []
      |otherwise = []
      where
        p1 = fst ((score b) !! 0)
        p2 = fst ((score b) !! 1)
        scoreP1 = snd ((score b) !! 0)
        scoreP2 = snd ((score b) !! 1)

score :: HijaraGame -> [(HijaraPlayer, Int)]
score (NewHijara matrix) = [(BluePlayer,diezPuntosAzul+quincePuntosAzul+veintePuntosAzul),
                        (YellowPlayer,diezPuntosAmarillo+quincePuntosAmarillo+veintePuntosAmarillo)]
                        where
                          diezPuntosAmarillo = contarDeADiezJugador 'y' (NewHijara matrix)
                          quincePuntosAmarillo = contarDeAQuinceJugador Yellow (NewHijara matrix)
                          veintePuntosAmarillo = contarDeAVeinteJugador Yellow (NewHijara matrix)
                          diezPuntosAzul = contarDeADiezJugador 'b' (NewHijara matrix)
                          quincePuntosAzul = contarDeAQuinceJugador Blue (NewHijara matrix)
                          veintePuntosAzul = contarDeAVeinteJugador Blue (NewHijara matrix)
                          
                

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
                                             horizontalesTablero = dividirDeAOcho (showGame (NewHijara matrix))
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


--Devuelve un string con las filas del tablero en orden
showGame :: HijaraGame -> String
showGame (NewHijara matrix) = foldr1 (++) (map show (pasarALista primerasFilas ))
    where
      horizontalesDeTablero = hLines matrix
      horizontalesDeMatriz = [(map hLines x) | x <- horizontalesDeTablero]
      primerasFilas = [(((horizontalesDeMatriz !! y) !! x) !! z) | y <- [0..3], z <-[0..1], x <- [0..3] ]

pasarALista :: [[Casilla]] -> [Casilla]
pasarALista [] = []
pasarALista lista = cabeza ++ (pasarALista cola)
            where
              cabeza = (foldr1 (++) (take 4 lista))
              cola = (drop 4 lista)

--Imprime el tablero en forma fácilmente legible
showHijaraPretty:: HijaraGame -> IO ()
showHijaraPretty (NewHijara m) = putStr (concat [ x ++ "\n" ++ y ++ "\n\n" | (x,y) <- zip listFilasParesToString listFilasImparesToString ])
  where
    filas = hLines m
    listFilasParesToString = [  (showLine True ((hLines ( x !! 0)) !! 0)) ++  "\t" ++ (showLine True ((hLines ( x !! 1)) !! 0)) ++  "\t" ++ (showLine True ((hLines ( x !! 2)) !! 0)) ++  "\t" ++ (showLine True ((hLines ( x !! 3)) !! 0)) | x <- filas ]
    listFilasImparesToString = [ (showLine False ((hLines ( x !! 0)) !! 1)) ++  "\t" ++ (showLine False ((hLines ( x !! 1)) !! 1)) ++  "\t" ++ (showLine False ((hLines ( x !! 2)) !! 1)) ++  "\t" ++ (showLine False ((hLines ( x !! 3)) !! 1)) | x <- filas ]

showLine :: Bool -> [Casilla] -> String
showLine top line = "|" ++ value1 ++ " " ++ value2 ++ "|"
  where
    value1 = if ((line !! 0) == Empty) then (if top then "1" else "3") else show (line !! 0)
    value2 = if ((line !! 1) == Empty) then (if top then "2" else "4") else show (line !! 1)


showAction :: HijaraAction -> String
showAction a = show a --TODO
   
readAction :: String -> HijaraAction
readAction a
        |length intList == 3 = NewAction (intList !! 0) (intList !! 1) (intList !! 2)
        |otherwise = error "El valor ingresado no es correcto"
        where
          intList = map (parseInt) (finalSplit ' ' a)

isFinished :: HijaraGame -> Bool
isFinished a = (p1 == []) && (p2 == [])
  where 
    p1 = snd ((actions a) !! 0)
    p2 = snd ((actions a) !! 1)

stringSplit :: Char -> String -> [String]
stringSplit _ "" = [""]
stringSplit ch (x:xs)
      | x == ch = "" : cola
      | otherwise = (x : head cola) : tail cola
      where
        cola = stringSplit ch xs

finalSplit :: Char -> String -> [String]
finalSplit ch st = filter (/="") (stringSplit ch st) 

parseInt :: String -> Int
parseInt "" = error "error!"
parseInt cadena = foldr1 (+) (getIntList cadena)

getIntList :: String -> [Int]
getIntList [] = []
getIntList (x:xs) = ((digitToInt x)*(10^(length xs))) : (getIntList xs)

activePlayer :: HijaraGame -> HijaraPlayer
activePlayer (NewHijara matrix) = if numeroB <= numeroY then (BluePlayer) else (YellowPlayer)
                      where
                        tablero = showGame (NewHijara matrix)
                        numeroY = length (filter (\x -> x == 'y') tablero)
                        numeroB = length (filter (\x -> x == 'b') tablero)

players :: [HijaraPlayer]
players = [BluePlayerm,YellowPlayer]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type HijaraAgent = HijaraGame -> IO (Maybe HijaraAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (HijaraAgent, HijaraAgent) -> HijaraGame -> IO [(HijaraPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showHijaraPretty g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust $ elemIndex p players)
         move <- ag g
         runMatch ags (Hijara.next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO [(HijaraPlayer, Int)]
runOnConsole = do
   runMatch (consoleAgent BluePlayer, consoleAgent YellowPlayer) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: HijaraPlayer -> HijaraAgent
consoleAgent player state = do
   let moves = fromJust $ lookup player (actions state)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: HijaraPlayer -> HijaraAgent
randomAgent player state = do
    let moves = fromJust $ lookup player (actions state)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))
