module Stack (Stack, -- exportar o tipo
        push, pop, top, -- e as operações
        empty, isEmpty) where

data Stack a = Stk [a]

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"


top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

parent :: String -> Bool
parent str = parentAux str empty


parentAux :: String -> Stack Char -> Bool
parentAux [] stk = isEmpty stk
parentAux (x:xs) stk
    | x == '(' = parentAux xs (push '(' stk)
    | x == ')' = not (isEmpty stk) &&
            top stk == '(' &&
            parentAux xs (pop stk)
    | otherwise = parentAux xs stk