-- 4.1
algarismos :: Int -> [Int]
algarismos n = reverse (algarismosAux n)

algarismosAux :: Int -> [Int]
algarismosAux n
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ algarismosAux (n `div` 10)

-- 4.2
toBits :: Int -> [Int]
toBits n = reverse (toBitsAux n)

toBitsAux :: Int -> [Int]
toBitsAux n
    | n < 2 = [n]
    | otherwise = [n `mod` 2] ++ toBitsAux (n `div` 2)

-- 4.3
fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = x*(2^(length (xs))) + fromBits xs

-- 4.4 
mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a `mod` b)


-- 4.5a)
myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (x:xs)
    | n <= x = [n, x] ++ xs
    | otherwise = [x] ++ myinsert n xs

-- 4.5b)
mysort :: Ord a => [a] -> [a]
mysort [] = []
mysort (x:xs) = myinsert x (mysort xs)


-- 4.6a)
myminimum :: Ord a => [a] -> a
myminimum (x:xs) = minimumAux x l
    where l = [x] ++ xs

minimumAux :: Ord a => a -> [a] -> a
minimumAux n [] = n
minimumAux n (x:xs)
    | x < n = minimumAux x xs
    | otherwise = minimumAux n xs

-- 4.6b)
delete :: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs) 
    | n == x = xs
    | otherwise = [x] ++ delete n xs

-- 4.6c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = [myminimum xs] ++ ssort (delete (myminimum xs) xs)

-- 4.7a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | y < x = [y] ++ merge (x:xs) ys
    | x < y = [x] ++ merge xs (y:ys)
    | otherwise = [x,y] ++ merge xs ys

-- 4.7b)
-- msort :: Ord a => [a] -> [a]
-- msort [] = []
-- msort [x] = [x]
