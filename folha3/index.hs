import Data.Char

-- 3.1
divprop :: Integer -> [Integer]
divprop n = [x | x<-[1..n-1], n `mod` x == 0]

-- 3.2
perfeitos :: Integer -> [Integer]
perfeitos n = [x | x<-[1..n], sum (divprop (x)) == x]

-- 3.3
pitagoricos :: Integer -> [(Integer ,Integer ,Integer)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], (x*x) + (y*y) == (z*z)]

-- 3.4
primo :: Integer -> Bool
primo n
        | divprop(n) == [1] = True
        | otherwise = False

-- 3.5



-- 3.6
-- type Rel [x] [(y,z)] = ([Int], [(Int,Int)])

-- 3.6a)



-- 3.7 a)
myand :: [Bool] -> Bool
myand [] = True 
myand (x:xs)
        | x == False = False
        | otherwise = myand xs

-- 3.7 b)
myor :: [Bool] -> Bool
myor [] = False
myor (x:xs)
        | x == True = True
        | otherwise = myor xs

-- 3.7c)
myconcat ::[[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

-- 3.7d)
myreplicate :: Int -> a -> [a] 
myreplicate 0 a = []
myreplicate n a = [a] ++ myreplicate (n-1) a 

-- 3.7e)
myrets :: [a] -> Int -> a
myrets (x:xs) 1 = x
myrets (x:xs) n = myrets (xs) (n-1)

-- 3.7f)
myelem :: Eq a => a -> [a] -> Bool
myelem x [] = False
myelem x (y:ys)
        | x == y = True
        | otherwise = myelem x ys

-- 3.9
minuscula :: String -> Bool
minuscula [] = False
minuscula (x:xs)
        | isLower x = True
        | otherwise = minuscula xs

maiuscula :: String -> Bool
maiuscula [] = False
maiuscula (x:xs)
        | isUpper x = True
        | otherwise = maiuscula xs

numero :: String -> Bool
numero [] = False
numero (x:xs)
        | x >= '0' && x <= '9' = True
        | otherwise = numero xs

forte :: String -> Bool
forte xs = myand[minuscula xs, maiuscula xs, numero xs, length(xs) > 7]

-- 3.11
--mynub :: Eq a => [a] -> [a]
--my

-- 3.12
intersperse :: a -> [a] -> [a]
intersperse a [] = []
intersperse a [x] = [x]
intersperse a (x:xs) = [x] ++ [a] ++ intersperse a xs