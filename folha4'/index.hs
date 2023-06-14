algarismos :: Int -> [Int]
algarismos 0 = []
algarismos n = algarismos (n `div` 10) ++ [n `mod` 10]


toBits :: Int -> [Int]
toBits 0 = []
toBits n = toBits (n `div` 2) ++ [n `mod` 2]


fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = x * 2^(length (xs)) + fromBits xs


mdc :: Integer -> Integer -> Integer
mdc a b 
    | b == 0 = a
    | otherwise = mdc b (a `mod` b)


insert' :: Integer -> [Integer] -> [Integer]
insert' n [] = [n]
insert' n (x:xs)
    | n < x = (n:x:xs)
    | otherwise = [x] ++ insert' n xs


--minimum' :: Ord a => [a] -> a
--minimum' xs 