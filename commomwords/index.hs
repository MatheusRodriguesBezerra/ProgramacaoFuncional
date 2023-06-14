import Data.List
import Data.Char

--commonWords :: Int -> String -> [(String, Int)]


minuscula :: String -> String
minuscula [] = []
minuscula (x:xs) = [toLower x] ++ minuscula xs


palavras :: String -> [String]
palavras xs = words xs


ordenar :: [String] -> [String]
ordenar xs = sort xs


contarOcorrecias :: [String] -> [(String,Int)]
contarOcorrecias [] = []
contarOcorrecias (w:ws) = (w, 1+length ws') : contarOcorrecias ws''
        where ws' = takeWhile (==w) ws
              ws''= dropWhile (==w) ws
