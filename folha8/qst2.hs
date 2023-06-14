import Stack

calcular_aux :: Stack Float -> [String] -> Float
calcular_aux st [] = top st
calcular_aux st ("+":xs) = calcular_aux stnova xs
                    where 
                        stnova = push res (pop (pop st))
                        res = a + b
                        a = top st
                        b = top(pop st)
calcular_aux st ("-":xs) = calcular_aux stnova xs
                    where 
                        stnova = push res (pop (pop st))
                        res = a - b
                        a = top st
                        b = top(pop st) 
calcular_aux st ("*":xs) = calcular_aux stnova xs
                    where 
                        stnova = push res (pop (pop st))
                        res = a * b
                        a = top st
                        b = top(pop st) 
calcular_aux st ("/":xs) = calcular_aux stnova xs
                    where 
                        stnova = push res (pop (pop st))
                        res = a `div` b
                        a = top st
                        b = top(pop st) 
calcular_aux st (num:xs) = calcular_aux (push (read num) st) xs

calcular :: String -> Float
calcular s = calcular_aux empty (words s)