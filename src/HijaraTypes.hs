module HijaraTypes
  where

import Data.Matrix (Matrix)

data HijaraPlayer = BluePlayer | YellowPlayer deriving (Eq, Show, Enum)
data HijaraGame = NewHijara (Matrix (Matrix Casilla))
data Casilla = Blue | Yellow | Empty deriving (Eq)
-- EL primer int indica la fila de la seccion, el segundo la columna de la seccion y el tercero el valor de la casilla posible
-- No existe una accion si no hay casilla posible
data HijaraAction = NewAction Int Int Int deriving (Eq, Show)

instance Show Casilla where
  show (Yellow) = "y"
  show (Blue) = "b"
  show (Empty) = "x"