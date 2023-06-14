intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse c [x] = [x]
intersperse c (x:xs) = x : c : intersperse c xs
