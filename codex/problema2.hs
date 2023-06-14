--          **CONCLUIDO!!!**


-- parte 1
decomporIni :: Int -> [Int]
decomporIni n
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ decomporIni (n `div` 10)


-- parte 2
multiplicar :: [Int] -> [Int]
multiplicar [] = []
multiplicar (x:[]) = [x]
multiplicar (x:y:xs) = [x] ++ [y*2] ++ multiplicar xs 


-- parte 3
soma :: [Int] -> Int 
soma [] = 0
soma (x:xs)
    | x >= 10 = (x `mod` 10) + (x `div` 10) + soma xs
    | otherwise = x + soma xs


-- parte 4
resultado :: Int -> Bool
resultado n 
    | n `mod` 10 == 0 = True
    | otherwise = False


-- resultado final
validar :: Int -> Bool
validar n = resultado (soma (multiplicar (decomporIni n)))