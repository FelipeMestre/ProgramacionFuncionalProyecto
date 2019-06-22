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

-- activePlayer :: HijaraGame -> HijaraPlayer 

-- actions :: HijaraGame -> [(HijaraPlayer, [HijaraAction])] 

-- next :: HijaraGame -> (HijaraPlayer, HijaraAction) -> HijaraGame

-- result :: HijaraGame -> [(HijaraPlayer, Int)]

-- score :: HijaraGame -> [(HijaraPlayer, Int)]



  showGame :: HijaraGame -> String
  showGame hijara =
    where
      algo = hLines $  fromList 4 4 [fromList 2 2 [Empty | x <- [1..4]] | _ <- [1..16]]
      lista = [(map hLines x) | x <- algo]
-- showConsole :: HijaraGame -> IO ()
-- showConsole hijara = putStr showGame hijara
-- showAction :: HijaraAction -> String

-- readAction :: String -> HijaraAction


hLines sud = [toList (getRow i sud) | i <- [1..nrows sud ]]

vLines sud = [toList (getCol i sud) | i <- [1..ncols sud ]]