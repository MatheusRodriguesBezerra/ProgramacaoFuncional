
--mediana :: Int -> Int -> Int -> Int
--mediana x y z = if x>=y && x<=z || x<=y && x>=z then x else
--                if y>=x && y<=z || y<=x && y>=z then y
  --              else z

mediana :: Int -> Int -> Int -> Int
mediana x y z = x+y+z - max3 x y z -min3 x y z
max3 x y z = if x>=max y z then x else max y z
min3 :: Int -> Int -> Int -> Int
min3 x y z = if x<=min y z then x else min y z
