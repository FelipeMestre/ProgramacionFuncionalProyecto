import Data.Matrix (Matrix, getCol, getRow, ncols, nrows, toList, toLists, fromLists, fromList, submatrix, getElem, setElem)
import Data.Vector (toList)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
data HijaraPlayer = BluePlayer | YellowPlayer deriving (Eq, Show, Enum)
data HijaraGame = NewHijara (Matrix (Matrix Casilla))
data Casilla = Blue | Yellow | Empty deriving (Eq)


-- EL primer int indica la fila de la seccion, el segundo la columna de la seccion y el tercero el valor de la casilla posible
-- No existe una accion si no hay casilla posible 
data HijaraAction = NewAction Int Int Int deriving (Eq, Show)

hLines sud = [Data.Vector.toList (getRow i sud) | i <- [1..nrows sud ]]

vLines sud = [Data.Vector.toList (getCol i sud) | i <- [1..ncols sud ]]


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

instance Show Casilla where
  show (Yellow) = "y"
  show (Blue) = "b"
  show (Empty) = "x"


beginning :: HijaraGame
beginning = NewHijara $ fromList 4 4 [fromList 2 2 [Empty | x <- [1..4]] | _ <- [1..16]]  

showGame :: HijaraGame -> String
showGame (NewHijara matrix) = foldr1 (++) (map show (pasarALista primerasFilas ))
    where
      algo = hLines matrix
      lista = [(map hLines x) | x <- algo]
      primerasFilas = [(((lista !! y) !! x) !! z) | y <- [0..3], z <-[0..1], x <- [0..3] ]  

pasarALista :: [[Casilla]] -> [Casilla]
pasarALista [] = []
pasarALista lista = cabeza ++ (pasarALista cola) 
            where
              cabeza = (foldr1 (++) (take 4 lista))
              cola = (drop 4 lista)


activePlayer :: HijaraGame -> HijaraPlayer
activePlayer (NewHijara matrix) = if numeroB >= numeroY then (BluePlayer) else (YellowPlayer) 
                      where
                        tablero = showGame (NewHijara matrix)
                        numeroY = length (filter (\x -> x == 'y') tablero)
                        numeroB = length (filter (\x -> x == 'b') tablero)


actions :: HijaraGame -> [(HijaraPlayer, [HijaraAction])]
actions (NewHijara higa) = [(actPlayer, listaMovimientos), (noActPlayer, [])]
                        where
                          actPlayer = activePlayer (NewHijara higa)
                          noActPlayer = if (activePlayer (NewHijara higa) == BluePlayer) then YellowPlayer else BluePlayer
                          filas = hLines higa
                          -- En la siguiente linea "x" va a iterar con cada fila, "y" solo indica el numero de la fila y "z" itera las columnas el numero de la columna
                          listaMovimientos = [ NewAction y z ((fromJust (elemIndex Empty (Data.Matrix.toList (x !! z)))) + 1) | (x,y) <- zip filas [0..3], z <- [0..3], Empty `elem` (Data.Matrix.toList (x !! z)) ]

next :: HijaraGame -> (HijaraPlayer, HijaraAction) -> HijaraGame
next (NewHijara hija) (player, NewAction fila columna valor)
  | not (player == (activePlayer (NewHijara hija))) = error "El jugador no es le jugador activo"
  | not ((NewAction fila columna valor) `elem` posiblePlayerActions) = error "No se puede efectuar la accion ingresada"
  | otherwise = NewHijara (setElem newSection (fila, columna) hija)
  where
    posiblePlayerActions = if (fst ((actions (NewHijara hija) !! 0)) == player) 
      then snd ((actions (NewHijara hija) !! 0))
      else snd ((actions (NewHijara hija) !! 1))
    casillaDelJugador = if (player == YellowPlayer) then Yellow else Blue
    oldListOfSeccion = Data.Matrix.toList (((hLines hija) !! fila) !! columna) 
    newSection = (fromList 2 2 [ if (valor == y) then casillaDelJugador else x | (x,y) <- zip oldListOfSeccion [1..] ])

result :: HijaraGame -> [(HijaraPlayer, Int)]
result b 
      |isFinished b = if scoreP1 > scoreP2 then [(p1,1),(p2,-1)] else if scoreP1 < scoreP2 then [(p1,-1),(p2,1)] else []
      |otherwise = []
      where
        p1 = fst ((score b) !! 0)
        p2 = fst ((score b) !! 0)
        scoreP1 = snd ((score b) !! 0)
        scoreP2 = snd ((score b) !! 1)

score :: HijaraGame -> [(HijaraPlayer, Int)]
score a = [] --TODO

showAction :: HijaraAction -> String
showAction a = show a --TODO

readAction :: String -> HijaraAction
readAction a = NewAction 3 3 3 --TODO

players :: [HijaraPlayer]
players = [] --TODO

isFinished :: HijaraGame -> Bool
isFinished a = (p1 == []) && (p2 == [])
  where 
    p1 = snd ((actions a) !! 0)
    p2 = snd ((actions a) !! 1)