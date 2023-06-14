-- questao 1
divprop :: Integer -> [Integer]
divprop n = [ x | x<-[1..n], n `mod` x == 0 && n /= x]

-- questao 2
perfeitos :: Integer -> [Integer]
perfeitos n = [ x | x<-[1..n-1], x == sum(divprop x)]

-- questao 3
pitagoricos :: Integer -> [(Integer ,Integer ,Integer)]
pitagoricos n = [ (x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*x + y*y == z*z]

-- questao 4
primo :: Integer -> Bool
primo n | divprop n == [1] = True
        | otherwise = False

-- questao 7a
myand :: [Bool] -> Bool
myand [] = True
myand [True] = True
myand [False] = False
myand (x:xs) = x && myand xs 

-- questao 7b
myor :: [Bool] -> Bool
myor [] = False
myor [True] = True
myor [False] = False
myor (x:xs) = x || myor xs 

-- questao 7c
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

-- questao 7d
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate x a = [a] ++ myreplicate (x-1) a

-- questao 7e 
my7e :: [a] -> Int -> a
my7e xs 1 = head xs
my7e (_:xs) n = my7e xs (n-1)


-- questao 7f
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem a (x:xs) 
        | a == x = True
        | otherwise = myelem a xs


-- quesao 9
forte :: String -> Bool
forte a = mais8 a && maiuscula'' a && minuscula'' a && algarismo a


maiuscula'' :: String -> Bool
maiuscula'' [] = False
maiuscula'' (x:xs)
        | x >= 'A' && x <= 'Z' = True
        | otherwise = maiuscula'' xs

minuscula'' :: String -> Bool
minuscula'' [] = False
minuscula'' (x:xs)
        | x >= 'a' && x <= 'z' = True
        | otherwise = minuscula'' xs

mais8 :: String -> Bool
mais8 x 
        | length x >= 8 = True
        | otherwise = False 

algarismo :: String -> Bool
algarismo [] = False
algarismo (x:xs)
        | x >= '0' && x <= '9' = True
        | otherwise = algarismo xs


-- questao 12
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse a (x:xs) = [x] ++ [a] ++ intersperse a xs 
