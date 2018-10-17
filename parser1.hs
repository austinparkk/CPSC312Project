csvArray = []

-- splitsep (==',') "3,5,"  => ["3","5",""]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

             
readcsv filename =
  do
    file <- readFile filename
    return [splitsep (==',') line| line <- splitsep (=='\n') file]

 

               