testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c|(a + b) > c && (a + c) > b && (c + b) > a = True
                    |otherwise = False


areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = (s * (s - a) * (s - b) * (s - c)) ** 0.5
    where s = (a + b + c)/2


metades :: [a] -> ([a],[a])
metades a = (primeiro, segundo) 
    where 
        meio = length a `div` 2
        primeiro = take meio a
        segundo = drop meio a

         

last' :: [a] -> a
last' l = x
    where
        x = head (reverse l)

last'' :: [a] -> a
last'' a = head(drop x a)
    where x = length a - 1 


init' :: [Int] -> [Int]
init' l = x
    where
        x = take ((length l)-1) l


init'' :: [Int] -> [Int]
init'' l = reverse x
    where
        x = tail (reverse l)


binom :: Integer -> Integer -> Integer
binom n k = x
    where
        x = (product[1..n]) `div` ((product[1..k]) * (product[1..(n - k)]))