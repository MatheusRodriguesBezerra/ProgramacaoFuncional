 mindiv :: Int -> Int
 mindiv x
 let xfloat (fromInteger x) :: Float in
 let xsqrt = sqrt float in
 let xsqrti = round sqrt in
  head ([ d | d<-[2..xsqrti]], x`mod` d ==0] ++ [x])

  isprime x = mindiv x == x
