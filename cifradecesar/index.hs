import Data.Char

-- converter letra para inteiro
letraInt :: Char -> Int
letraInt x = ord x - ord 'A'


-- converter inteiro para letra
intLetra :: Int -> Char
intLetra n = chr (n + ord 'A')


maiuscula :: Char -> Bool
maiuscula a = a >= 'A' && a <= 'Z'


deslocar :: Int -> Char -> Char
deslocar k x 
    | maiuscula x = intLetra ((letraInt x + k) `mod` 26)
    | otherwise = x


cifrar :: Int -> String -> String
cifrar k xs = [deslocar k x | x<-xs]