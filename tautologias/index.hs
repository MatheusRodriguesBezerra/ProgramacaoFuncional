--representar proposições
data Prop = Const Bool          -- constante
            | Var Char          -- variável
            | Neg Prop          -- negação
            | Conj Prop Prop    -- conjunção
            | Disj Prop Prop    -- disjunção
            | Impl Prop Prop    -- implicação
            deriving (Eq,Show)


-- calcular o valor de uma proposição
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k assocs = head [v | (k',v)<-assocs, k==k']

type Atrib = Assoc Char Bool
valor :: Atrib -> Prop -> Bool
valor s (Const b) = b
valor s (Var x) = find x s
valor s (Neg p) = not (valor s p)
valor s (Conj p q) = valor s p && valor s q
valor s (Disj p q) = valor s p || valor s q
valor s (Impl p q) = not (valor s p) || valor s q