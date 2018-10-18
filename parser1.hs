
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
    let res = user elems -- user is the function that is called by the user. so elems is the argument for that function. 
    return res
    
    
----------------------------------------------------
--Functions: 

-- returns row r in the CSV
select_row :: Int -> [a] -> a
select_row r elems = elems !! r

-- returns columns r in the CSV
select_column :: Int -> [[a]] -> [a]
select_column r [] = []
select_column r (h:t) = (h !! r):select_column r t  

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

-- returns value in row r and column c as Char
select_value (r,c) elems = (select_row r elems) !! c

-- returns value in row r and column c by casting the Char to type Int
read_int :: (Int, Int) -> [[[Char]]] -> Int
read_int (r,c) elems = read ((select_row r elems) !! c)

-- given a cell index returns the row header
row_name :: (Int, b) -> [[a]] -> a
row_name (r,c) elems = (select_row r elems) !! 0

--given a cell index returns the column header
column_name :: (a1, Int) -> [[a2]] -> a2
column_name (r,c) elems = (select_column c elems) !! 0

--given a cell index returns the row header and column header as a pair 
-- FLAG There may be a prettier way to implement this function
row_column_name :: (Int, Int) -> [[b]] -> (b, b)
row_column_name (r,c) elems = zip [(select_row r elems) !! 0] [(select_column c elems) !! 0] !! 0                                        

--compare any two cell values and returns the coordinates of the greater value 
compare_values :: (Int, Int) -> (Int, Int) -> [[[Char]]] -> (Int, Int)
compare_values (r1,c1) (r2,c2) elems = (if (read_int (r1,c1) elems) > (read_int (r2,c2) elems) then (r1,c1) else (r2,c2))

-- compares two column values given a specific row, and returns the corresponding header
compare_columns :: Int -> Int -> Int -> [[[Char]]] -> [Char]
compare_columns c1 c2 r elems = column_name (compare_values (r,c1) (r,c2) elems) elems

-- compares two row values given a specific column, and returns the corresponding header
--compare_rows r1 r2 c f elems = 

--SUMIF

--COUNTIF

--CONCATENATE
---------

go = readcsv "HateCrimesByRegion2016.csv"
user  = max_row 7

--user  = average_columns 3
--user = select_row 1
--user = select_column 0 
--user = select_value (2,4)
--user = read_value (2,4) 
--user = compare_values (1,1) (2,2)
--user = row_column_name (1,1)
--user = compare_values (2,2) (2,3) 
--user = compare_columns 2 3 4 
--user = column_name (0,8)

               
