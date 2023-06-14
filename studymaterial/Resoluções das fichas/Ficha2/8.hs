converte :: Int -> String
converte



converte2 :: Int -> String
converte2 n = combina2 (decompor2 n)

decompor2 :: Int -> (Int,Int)
decompor2  n =(n `div` 10, n `mod` 10)
