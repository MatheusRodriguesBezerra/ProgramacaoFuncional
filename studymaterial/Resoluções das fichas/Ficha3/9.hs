forte :: String -> Bool
forte palavra = (length palavra >=8) && (maiuscula palavra) && (minuscula palavra) && (algarismo palavra)

maiuscula :: String -> Bool
maiuscula caracter = or [x=>'A' && x<='Z' | x<-caracter]

minuscula :: String -> Bool
maiuscula caracter = or [x=>'a' && x<='z' | x<-caracter]

algarismo :: String -> Bool
maiuscula caracter = or [x=>'0' && x<='9' | x<-caracter]
