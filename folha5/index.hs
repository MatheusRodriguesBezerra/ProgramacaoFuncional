-- questao 1
divisores :: Int -> [Int]
divisores n = filter (\x->n `mod` x == 0) [1..(n-1)]


-- questao 2
--primo :: Integer -> Bool



-- questao 3a
qs3a' :: [a] -> [a] -> [a]
qs3a' xs ys = xs ++ ys

qs3a :: [a] -> [a] -> [a]
qs3a xs ys = foldr (:) ys xs


-- questao 3b
concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs

--concat' :: [[a]] -> [a]
--concat' xs = foldr (:) xs


-- questao 3c
reverse3c :: [a] -> [a]
reverse3c [] = []
reverse3c (x:xs) = reverse3c xs ++ [x]


-- questao 3d


-- questao 3e
elem' :: Eq a => a -> [a]-> Bool
elem' _ [] = False
elem' n (x:xs) 
    | n == x = True
    | otherwise = elem' n xs

elem'' :: Eq a => a -> [a]-> Bool
elem'' n (x:xs) = any (\y->y == n) xs 


-- questao 6


-- questao 7