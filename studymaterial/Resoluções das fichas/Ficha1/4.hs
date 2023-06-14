metades :: [a] -> ([a],[a])
metades lista = (primeira,segunda)
              where
              meio = length lista `div` 2
              primeira = take meio lista
              segunda = drop meio lista
