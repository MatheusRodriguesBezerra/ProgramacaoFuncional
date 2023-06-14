-- curta :: [a] -> Bool
 --curta xs | length xs<=2 = True
  --        | otherwise = False

curta :: [a] -> Bool
curta [] = True
curta [_]=True
curta[_,_]=True
curta x= False
