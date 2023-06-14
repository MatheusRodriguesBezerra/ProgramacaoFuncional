--a) reverse e head
--last' :: [a] -> a
--last' xs = head (reverse xs)
--drop,length e head
--last' xs = head $ drop (length xs -1) xs
--last' xs = xs !! (length xs -1)
--b)
init' :: [a] -> [a]
init' xs = reverse (tail(reverse lista))

init' xs = take (length-1 lista) lista
