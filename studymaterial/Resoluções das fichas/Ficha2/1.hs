{--classifica :: Int -> String
classifica nota = if nota>=18 then "excelente" else
                  if nota>=16 then "muito bom" else
                  if nota>=13 then "bom" else
                  if nota>=10 then "suficiente" else "reprovado"
--}
classifica nota | nota>=18 = "excelente"
                | nota>=16 = "muito bom"
                | nota>=13 = "bom"
                | nota>=10 = "sufuciente"
                | otherwise = "reprovado"
