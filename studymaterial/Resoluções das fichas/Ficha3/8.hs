myreplicate :: Int -> a -> [a]
myreplicate n xs = [xs | xs <-[1..n]]

myconcat :: [[a]] -> [a]
myconcat xs = [val | x <- xs, val<-xs]

selecon :: [a] -> Int -> a
selecon lista n = head [ x | (x,y) <- zip lista [0..n], y == n]
