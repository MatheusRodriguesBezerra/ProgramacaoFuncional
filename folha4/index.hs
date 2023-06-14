-- questao 1
algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n = [n `mod` 10] ++ algarismosRev (n`div` 10)

algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)


-- questao 2
toBitsRev :: Int -> [Int]
toBitsRev 0 = []
toBitsRev n = [n `mod` 2] ++ toBitsRev (n`div` 2)

toBits :: Int -> [Int]
toBits n = reverse (toBitsRev n)


-- questao 3
fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = x * 2^(length xs) + fromBits xs


-- questao 4
mdc :: Integer -> Integer -> Integer
mdc a b 
    | b == 0 = a
    | otherwise = mdc b (a `mod` b)


-- questao 5
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (x:xs) 
    | n < x = [n] ++ x:xs
    | otherwise = [x] ++ insert' n xs

isort' :: Ord a => [a] -> [a] 
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)


-- questao 6
minimum' :: Ord a => [a] -> a
minimum' xs = head (isort' xs)

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) 
    | n == x = xs
    | otherwise = [x] ++ delete' n xs

ssort' :: Ord a => [a] -> [a]
ssort' [] = []
ssort' xs = [minimum' xs] ++ ssort' (delete' (minimum' xs) xs) 


-- questao 7
merge :: Ord a => [a] -> [a] -> [a] 
merge xs ys = ssort' (xs++ys) 


-- questao 8 
addPoly :: [Int] -> [Int] -> [Int]
addPoly [] [] = []
addPoly xs [] = xs
addPoly [] ys = ys
addPoly (x:xs) (y:ys) = [x+y] ++ addPoly xs ys

