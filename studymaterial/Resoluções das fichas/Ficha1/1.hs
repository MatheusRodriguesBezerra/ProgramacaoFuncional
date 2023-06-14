{--1.1)
a) incr(triplo 3) = incr ( 3*3) = incr(9) =9+1= 10
b) triplo (incr 3) = triplo ( 3+1)= triplo 4= 3*4=12
c) boasVindas "Linguagem" ++ " Haskell"
Olá,Linguagem! Haskell
d) boasVindas ("Linguagem" ++ " Haskell")
OLá,LinguagemHaskell!
e) boasVindas (boasVindas "Haskell")
boasVindas(Olá,Haskell!)=Olá,Olá,Haskell!!
--}

incr, triplo :: Integer -> Integer
incr x = x+1
triplo x = 3*x
boasVindas :: String -> String
boasVindas nome = "Olá, " ++ nome ++ "!"
