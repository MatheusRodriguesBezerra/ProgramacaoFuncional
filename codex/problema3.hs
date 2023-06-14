--          **CONCLUIDO!!!**


-- parte 1
decompor :: Int -> [Int]
decompor n 
    | n `div` 200 > 0 = [200] ++ decompor (n-200)
    | n `div` 100 > 0 = [100] ++ decompor (n-100)
    | n `div` 50 > 0 = [50] ++ decompor (n-50)
    | n `div` 20 > 0 = [20] ++ decompor (n-20)
    | n `div` 10 > 0 = [10] ++ decompor (n-10)
    | n `div` 5 > 0 = [5] ++ decompor (n-5)
    | otherwise = []


-- parte 2 
decomporPt1 :: Int -> [Int] -> [Int]
decomporPt1 _ [] = []
decomporPt1 n (x:xs) 
    | n >= x = [x] ++ decomporPt1 (n-x) xs
    | otherwise = [] ++ decomporPt1 n xs

decomporPt2 :: Int -> [Int] -> [Int]
decomporPt2 _ [] = []
decomporPt2 n (x:xs)
    | n >= x = [] ++ decomporPt2 (n-x) xs
    | otherwise = [x] ++ decomporPt2 n xs

decomporTrans :: Int -> [Int] -> ([Int],[Int])
decomporTrans n xs = (decomporPt1 n xs, decomporPt2 n xs) 


-- parte 3
transacao :: (Int,[Int]) -> [Int] -> ([Int],[Int])
transacao (n, xs) ys = (resultadoPt1 (n, xs) ys, resultadoPt2 (n, xs) ys)

troco :: Int -> [Int] -> Int
troco n xs = (sum xs) - n

resultadoPt1 :: (Int,[Int]) -> [Int] -> [Int]
resultadoPt1 (n, xs) ys
    | troco n xs >= 0 = decomporPt1 (troco n xs) (xs++ys)
    | otherwise = xs

resultadoPt2 :: (Int,[Int]) -> [Int] -> [Int]
resultadoPt2 (n, xs) ys
    | troco n xs >= 0 = decomporPt2 (troco n xs) (xs++ys)
    | otherwise = ys
