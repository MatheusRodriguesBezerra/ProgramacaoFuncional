product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs


qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = qs menores ++ [x] ++ qs maiores
    where menores = [ y | y<-xs, y<=x]
          maiores = [ y | y<-xs, y>x]


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)



{- 

1.1
a)

-}