 mynub :: Eq a => [a] -> [a]
 mynub [] = []
 mynub (x:xs) = x: mynub(apaga x xs)
  where
    apaga c l = [y | y<-l, y/=c]
