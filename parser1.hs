
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

-- returns row r in the CSV
select_row :: Int -> [a] -> a
select_row r elems = elems !! r

-- returns columns r in the CSV
select_column :: Int -> [[a]] -> [a]
select_column r [] = []
select_column r (h:t) = (h !! r):select_column r t  

-- returns the sum of row r in the CSV
sum_row :: Int -> [[String]] -> Int
sum_row r elems = sum [read e :: Int | e <- tail (elems !! r)]

-- returns the sum of column r in the CSV
sum_column :: Int -> [[String]] -> Int
sum_column r elems = sum [read e :: Int | e <- tail (select_column r elems)]

-- returns the average of row r in the CSV
average_row :: Fractional a => Int -> [[String]] -> a
average_row r elems = (fromIntegral (sum_row r elems)) / (fromIntegral ((length (head elems)) - 1))

-- returns the sum of column r in the CSV
average_columns :: Fractional a => Int -> [[String]] -> a
average_columns r elems = (fromIntegral (sum_column r elems)) / (fromIntegral ((length (select_column r elems)) - 1))

min_row :: Int -> [[String]] -> Int
min_row r elems = minimum (map (\ x ->  read x :: Int) (tail (select_row r elems)))

max_row :: Int -> [[String]] -> Int
max_row r elems = maximum (map (\ x ->  read x :: Int) (tail (select_row r elems)))

min_column :: Int -> [[String]] -> Int
min_column r elems = minimum (map (\ x ->  read x :: Int) (tail (select_column r elems)))

max_column :: Int -> [[String]] -> Int
max_column r elems = maximum (map (\ x ->  read x :: Int) (tail (select_column r elems)))
---------

go = readcsv "HateCrimesByRegion2016.csv"
user  = max_row 7
               
