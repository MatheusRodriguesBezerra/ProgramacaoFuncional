 myand :: [Bool] -> Bool
 myand [] = True
 myand (False:xs) = False
 myand (True:xs)  = myand xs


{--myor :: [Bool] -> Bool
myor [] = False
myor (True:xs) = True
myor (False:xs) = myor xs


myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs
--}

myreplicate :: Int -> a -> [a]
myreplicate  n xs | n <=0 = []
                  | otherwise = n : myreplicate (n-1) xs



selecon :: [a] -> Int -> a
selecon (x:_) 0 = x
selecon (_:xs) n = selecon xs (n-1)



myelem :: Eq a => a -> [a] -< Bool
myelem _ [] = False
myelem n (x:xs) | x == n = True
                | otherwise myelem n xs
