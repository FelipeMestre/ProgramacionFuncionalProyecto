import Data.Matrix (Matrix, getCol, getRow, ncols, nrows, toLists, fromLists, fromList, submatrix, getElem)
import Data.Vector (toList)
import Data.List (sort)

data HijaraPlayer = BluePlayer | YellowPlayer deriving (Eq, Show, Enum)
data HijaraGame = NewHijara (Matrix (Matrix Casilla))
data Casilla = Blue | Yellow | Empty



instance Show Casilla where
  show (Yellow) = "y"
  show (Blue) = "b"
  show (Empty) = "x"


beginning :: HijaraGame
beginning = NewHijara $ fromList 4 4 [fromList 2 2 [Empty | x <- [1..4]] | _ <- [1..16]]  

showGame :: HijaraGame -> String
showGame (NewHijara matrix) = foldr1 (++) (map show (pasarALista primerasFilas))
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
                        numeroB = length (filter (\x -> x == 'y') tablero)


actions :: HijaraGame -> [(HijaraPlayer, [HijaraAction])]


-- next :: HijaraGame -> (HijaraPlayer, HijaraAction) -> HijaraGame

-- result :: HijaraGame -> [(HijaraPlayer, Int)]

-- score :: HijaraGame -> [(HijaraPlayer, Int)]




       
-- showConsole :: HijaraGame -> IO ()
-- showConsole hijara = putStr showGame hijara
-- showAction :: HijaraAction -> String

-- readAction :: String -> HijaraAction


hLines sud = [toList (getRow i sud) | i <- [1..nrows sud ]]

vLines sud = [toList (getCol i sud) | i <- [1..ncols sud ]]