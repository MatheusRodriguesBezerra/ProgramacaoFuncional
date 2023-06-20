-- 1.2
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c|(a + b) > c && (a + c) > b && (c + b) > a = True
                    |otherwise = False


-- 1.3
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = (s * (s - a) * (s - b) * (s - c)) ** 0.5
    where s = (a + b + c)/2


-- 1.4
metades :: [a] -> ([a],[a])
metades a = (primeiro, segundo) 
    where 
        meio = length a `div` 2
        primeiro = take meio a
        segundo = drop meio a


-- 1.5a)
last :: [a] -> a
last l = head (reverse l)

-- 1.5b)
init :: [Int] -> [Int]
init l = reverse (tail (reverse l))


-- 1.6
binom :: Integer -> Integer -> Integer
binom n k = x
    where
        x = (product[1..n]) `div` ((product[1..k]) * (product[1..(n - k)]))