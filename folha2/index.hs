-- 2.1
classifica :: Int -> String
classifica n 
    | n <= 9 = "reprovado"
    | n <= 12 = "suficiente"
    | n <= 15 = "bom"
    | n <= 18 = "muito bom"
    | otherwise = "muito bom com distincao"

-- 2.2
classifica2 :: Float -> Float -> String
classifica2 peso altura 
    | (peso / (altura*altura)) <= 18.5 = "baixo peso"
    | (peso / (altura*altura)) <= 25 = "peso normal"
    | (peso / (altura*altura)) <= 30 = "excesso de peso"
    | otherwise = "obesidade"

-- 2.3
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z
    | x > y && x > z = x
    | y > z = y
    | otherwise = z
min3 x y z
    | x < y && x < z = x
    | y < z = y
    | otherwise = z

-- 2.4
xor :: Bool -> Bool -> Bool
xor x y
    | x == False && y == True = True
    | x == True && y == False = True
    | otherwise = False

-- 2.5
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

-- 2.6
curta :: [a] -> Bool
curta x
    | length(x) > 2 = False
    | otherwise = True

-- 2.7 
mediana :: Int -> Int -> Int -> Int
mediana x y z = sum([x,y,z]) - min3 x y z - max3 x y z