classifica :: Int -> String
classifica n 
    | n <= 9 = "reprovado"
    | n <= 12 = "suficiente"
    | n <= 15 = "bom"