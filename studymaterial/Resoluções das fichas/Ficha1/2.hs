testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = if a < b+c && b < a+c && c < a+b then True else False
