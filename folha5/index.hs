-- 5.1
divisores :: Integer -> [Integer]
divisores n = filter (\i-> n `mod` i == 0) [1..n]


-- 5.9i)
aproxPi1 :: Int -> Double
aproxPi1 n = aproxPi1Aux (num n) (den n)

aproxPi1Aux :: [Double] -> [Double] -> Double
aproxPi1Aux [] [] = 0
aproxPi1Aux (x:xs) (y:ys) = (x/y) + aproxPi1Aux xs ys


num :: Int -> [Double]
num n = take n (iterate ((-1)*) 4) 

den :: Int -> [Double]
den n = take n (iterate (+2) 1)
    
-- 5.9ii)
aproxPi2 :: Int -> Double
aproxPi2 n = 3 + (aproxPi2Aux (num (n-1)) (den2 (n-1)))


aproxPi2Aux :: [Double] -> [Double] -> Double
aproxPi2Aux [] [] = 0
aproxPi2Aux (x:xs) (y:ys) = (x/y) + aproxPi2Aux xs ys

den2 :: Int -> [Double]
den2 n = take n (fat 2)

fat :: Double -> [Double]
fat n = [n*(n+1)*(n+2)] ++ fat (n+2)