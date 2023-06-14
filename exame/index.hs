
-- Quesão 1

--approxE :: Int -> Double
--approxE 0 = 1.0
--approxE n = 1.0 `div` product[1..n] + approxE (n-1)



-- Questão 2

agrupar ::  Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs) = [takeWhile (x==) (x:xs)] ++ agrupar (dropWhile (x==) (x:xs)) 




data Set a = Empty -- árvore vazia
    | Node a (Set a) (Set a) -- nó com duas sub-árvores
    deriving (Eq, Show)

insert' :: Ord a => a -> Set a -> Set a
insert' n (Empty) = Node n (Empty) (Empty)
insert' n (Node x y z)
    | n == x = Node x y z
    | n > x = Node x y (insert' n z)
    | n < x = Node x (insert' n y) z



member' :: Ord a => a -> Set a -> Bool
member' n (Empty) = False
member' n (Node x y z)
    | n == x = True
    | n > x = member' n z
    | n < x = member' n y



data Expr = Const Int -- constantes 1, 2, 3, etc
    | Var Char -- variáveis 'a', 'b', 'c', etc
    | Add Expr Expr -- soma de duas sub-expressões
    | Mult Expr Expr -- multiplicação de duas sub-expressões
    deriving (Eq, Show)

simplifica :: Expr -> Expr
simplifica (Add (Const x) (Const y)) = Const (x+y)
simplifica (Add (Var x) (Const y)) = Add (Var x) (Const y)
simplifica (Add (Const x) (Var y)) = Add (Const x) (Var y)
simplifica (Add (Var x) (Var y)) = Add (Var x) (Var y)
simplifica (Mult (Const x) (Const y)) = Const (x*y)
simplifica (Mult (Var x) (Const y)) = Mult (Var x) (Const y)
simplifica (Mult (Const x) (Var y)) = Mult (Const x) (Var y)
simplifica (Mult (Var x) (Var y)) = Mult (Var x) (Var y)
simplifica (Mult x y) = (Mult (simplifica x) (simplifica y))
simplifica (Add x y) = (Add (simplifica x) (simplifica y))
simplifica (Const x) = (Const x)
simplifica (Var x) = (Var x)

{-- 

(Add (Const 1) (Add (Const 2) (Var 'x'))

simplifica (Add (simplifica x) (simplifica y))

simplifica (Add (Const 1) (Add (Const 2) (Var 'x')))

simplifica 


--}



-- simplifica (Add (Const 2) (Mult (Const 3) (Const 5)))

