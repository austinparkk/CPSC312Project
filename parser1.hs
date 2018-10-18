import Data.Typeable
csvArray = []

-- splitsep (==',') "3,5,"  => ["3","5",""]
-- splitsep delimiter ignore list
splitsep _ _ [] = [[]]
splitsep sep ig (h:t)
    | sep h = []: splitsep sep ig t
    | ig h = splitsep sep ig t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep ig t

fls x = False
readcsv filename =
  do
    file <- readFile filename
    --let csvArray = [splitsep (==',') line| line <- splitsep (=='\n') file]
    --print (typeOf csvArray)
    let elems = [splitsep (==',') (`elem` "\160\r\65279") line| line <- splitsep (=='\n') fls file]
    let res = user elems
    return res

select_row r elems = elems !! r
sum_row r elems = sum [read e :: Int | e <- tail (elems !! r)]

---------------------------------------------------------------------------------------------------

go = readcsv "HateCrimesByRegion2016.csv"
user  = sum_row 3
               
