cifrar :: Int -> String -> String
cifrar k xs = [ desloca k xs | x<-xs]

desloca k x | maiuscula x = intLetra ((letraInt x + k) `mod` 26)
            | otherwise = x

maiuscula :: Char -> Bool
maiuscula x = x>='A' && x<='Z

letraInt :: Char -> Int
letraInt x = ord x - ord 'A'

intLetra :: Int -> Char
intLetra n = chr (n + ord 'A')
