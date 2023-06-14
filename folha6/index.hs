data Dir = Esquerda | Direita | Cima | Baixo
            deriving (Show)

direções :: [Dir]
direções = [Esquerda, Direita, Cima, Baixo]

oposta :: Dir -> Dir
oposta Esquerda = Direita
oposta Direita = Esquerda
oposta Cima = Baixo
oposta Baixo = Cima