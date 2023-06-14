--          **CONCLUIDO!!!**



pt1 :: (Ord a, Show a, Num a) => a -> a -> a -> a -> [Char]
pt1 horaini minutoini horafim minutofim
    |(horaini == horafim) && (minutofim>minutoini) && ( minutofim - minutoini > 1) = "Passaram apenas " ++ show(x) ++ " minutos!"
    |(horaini == horafim) && (minutofim>minutoini) && ( minutofim - minutoini == 1) = "Passou apenas 1 minuto!"
    |otherwise = "Passaram apenas " ++ show(x) ++ " minutos!"
    where x = (horafim*60+minutofim)-(horaini*60+minutoini)

pt2 :: (Integral a, Show a) => a -> a -> a -> a -> String
pt2 horaini minutoini horafim minutofim
    | x < 60= "De facto!"
    |x >= 120 && x `rem` 60 > 1= "Queres dizer, " ++ show(x `div` 60) ++ " horas e " ++ show(x `rem` 60) ++ " minutos?!"
    |x >= 120 && x `rem` 60 == 1= "Queres dizer, " ++ show(x `div` 60) ++ " horas e 1 minuto?!"
    |x == 60 = "Queres dizer, 1 hora?!"
    |x >= 120 && x `rem` 60 == 0 = "Queres dizer, " ++ show(x `div` 60) ++ " horas?!"
    |x > 61 && x < 120 = "Queres dizer, 1 hora e " ++ show(x `rem` 60) ++ " minutos?!"
    |x == 61= "Queres dizer, 1 hora e 1 minuto?!"
    where x = (horafim*60+minutofim)-(horaini*60+minutoini)


dialogo :: Int -> Int -> Int -> Int -> ([Char], String)
dialogo horaini minutoini horafim minutofim = (pt1 horaini minutoini horafim minutofim , pt2 horaini minutoini horafim minutofim)