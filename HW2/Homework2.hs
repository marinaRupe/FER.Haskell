import Data.List

--1. a)
isWellFormed :: [[Int]] -> Bool
isWellFormed xss = not $ ( False `elem` (checkLengths xss) || (null $ checkLengths xss))

checkLengths :: [[Int]] -> [Bool]
checkLengths xss = [ length xs == (length $ (head xss)) | xs <- xss, not $ (null $ head xss)]

--1. b)
size :: [[Int]] -> (Int, Int)
size xss = if isWellFormed xss then (length xss, length $ head xss) else error "Matrix is malformed"

--1. c)

noOfRows xss = fst $ size xss
noOfColumns xss = snd $ size xss

maxRow xss = (noOfRows xss) - 1
maxCol xss = (noOfColumns xss) - 1


getElement :: [[Int]] -> Int -> Int -> Int
getElement xss row col = if (rowInsideBounds && colInsideBounds)
    then getAtIndex col $ (getAtIndex row xss)
    else error "Index out of bounds"
      where rowInsideBounds = row <= maxRow xss && row >= 0
            colInsideBounds = col <= maxCol xss && col >= 0

index xs = zip xs [0..]

getAtIndex :: Int -> [a] -> a
getAtIndex n xs = head $ [ x | (x, i) <- index xs, i == n ]
 

--1. d)
getRow :: [[Int]] -> Int -> [Int]
getRow xss row = [ getElement xss row col | col <- [0..maxCol xss]]

--1. e)
getCol :: [[Int]] -> Int -> [Int]
getCol xss col = [ getElement xss row col | row <- [0..maxRow xss]]

--1. f)
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xss yss = if size xss /= size yss
    then error "Matrices are not of equal size"
    else [ [ (getElement xss i j) + (getElement yss i j) | j <- [0..maxCol xss] ] | i <- [0..maxRow xss ] ]

--1. g)
transpose' :: [[Int]] -> [[Int]]
transpose' xss = [ [ getElement xss i j | i <- [0..maxRow xss] ] | j <- [0..maxCol xss] ]
     
--1. h)
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xss yss = if noOfColumns xss /= noOfRows yss
  then error "Incompatible matrix dimensions"
  else [ [ sum $ multRowWithCol (getRow xss j) (getCol yss i) | i <- [0..maxCol yss]] | j <- [0..maxRow xss] ]


multRowWithCol xs ys = [ x*y | (x,j) <- index xs, (y,i) <- index ys, i==j]


--2.
type Key = Int
type Value = String
type Entry = (Key, Value)
type Dictionary = [Entry]
type Frequency = [(Value, Int)]

--2. a)
exists :: Key -> Dictionary -> Bool
exists key dict = key `elem` getKeys dict

getKeys dict = [ k | (k, v) <- dict]
getValues dict = [ v | (k, v) <- dict]

--2. b)
get :: Dictionary -> Key -> Value
get dict key = if null valuesForGivenKey
    then error ("key " ++ show key ++ " not found")
    else head $ valuesForGivenKey
        where valuesForGivenKey = [ v | (k, v) <- dict, k == key]

--2. c)
insert :: Entry -> Dictionary -> Dictionary
insert (key, value) dict = if key `elem` getKeys dict
  then [ if k == key then (k, value) else (k, v) | (k, v) <- dict]
  else (key, value) : dict

--2. d)
delete :: Key -> Dictionary -> Dictionary
delete key dict = [ (k, v) | (k, v) <- dict, k /= key]

--2. e)
freq :: Dictionary -> Frequency
freq dict = if null dict
  then error "dictionary is empty"
  else nub $ [ (value, f)  | value <- getValues dict, let f = sum $ [ 1 | (k, v) <- dict, v == value] ]

--3.
largestMultiple :: String -> Int
largestMultiple n = if null listOfMultiples
  then error "No such number"
  else head listOfMultiples
      where listOfMultiples = [ read p :: Int | p <- descPermutations n, isMultipleOf30 (read p :: Int) ]

isMultipleOf30 n = n `mod` 30 == 0

descPermutations n = reverse $ sort $ permutations n


--4.
undefined' :: a
undefined' = error "Prelude.undefined"

undefined'' :: a
undefined'' = undefined''

foo ::  a -> b -> a
foo x y = x  -- it works because x and y doesn't have to be the same type, it returns x so the returning type is type of x