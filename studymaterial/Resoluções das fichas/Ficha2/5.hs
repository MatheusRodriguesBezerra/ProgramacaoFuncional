 --safetail :: [a] -> [a]
 --safetail xs = if (length xs==0) then [] else tail xs


  --safetail :: [a] -> [a]
--  safetail [] = []
  --safetial (x:xs) = xs

  safetail :: [a] -> [a]
  safetail xs | length xs ==0 = []
              | otherwise = tail xs
