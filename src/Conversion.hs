
module Conversion 
  where
import HijaraTypes
import Data.Char
import Data.Matrix (Matrix, ncols, nrows, getRow, getCol)
import Data.Vector (toList)

pasarALista :: [[Casilla]] -> [Casilla]
pasarALista [] = []
pasarALista lista = cabeza ++ (pasarALista cola)
            where
              cabeza = (foldr1 (++) (take 4 lista))
              cola = (drop 4 lista)

hLines :: Matrix a -> [[a]]
hLines sud = [toList (getRow i sud) | i <- [1..nrows sud ]]

vLines :: Matrix a -> [[a]]
vLines sud = [toList (getCol i sud) | i <- [1..ncols sud ]]
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