import Data.Char
import Data.List
import Data.Ord (comparing)

-- === * LECTURE 7 * ===============================================================

-- === EXERCISE 1 ===============================================================
--1.1
takeThree = take 3
dropThree = drop 3
hundredTimes = replicate 100

--1.2
index = zip [0..]
index' = (`zip` [0..])

--1.3
divider :: Int -> [Char]
divider = (`replicate` '=')

-- === EXERCISE 2 ===============================================================
--2.1
applyOnLast f xs ys = f (last xs) (last ys)

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast (addThree 100)

addThree x y z = x + y + z

--2.2
applyManyTimes 0 _ x = x
applyManyTimes n f x = applyManyTimes (n-1) f (f x)

finishSentence = (++ ".")

applyTwice = applyManyTimes 2

-- === EXERCISE 3 ===============================================================
--3.1
listifylist :: [a] -> [[a]]
listifylist = map (:[])

--3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (\x -> if x >=n then n else x)

-- === EXERCISE 4 ===============================================================
--4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares s = sum $ map (^2) $ filter (even) s

--4.2
freq :: Eq a => a -> [a] -> Int
freq elem xs = length $ (filter (== elem) xs)

--4.3
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\x -> freq x xs >= n) xs

-- === EXERCISE 5 ================================================================
--5.1
withinInterval n m xs = filter (\x -> x >= n && x <=m) xs

--5.2
sndColumn = map (\(x:y:_) -> y)

--5.3
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs xs = map (switch) $ (filter (\(x,y) -> x /= y) xs)

switch (x,y) = (min x y, max x y)


-- === * LECTURE 8 * ===============================================================

-- === EXERCISE 1 ===============================================================
--1.1
sumEven :: [Integer] -> Integer
sumEven = sum . map (snd) . filter (even . fst) . zip [0..]

--1.2
filterWords :: [String] -> String -> String
filterWords ws =  unwords . filter (not . (`elem` ws)) . words


-- === EXERCISE 2 ===============================================================
--2.1
maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (uncurry (-)) $ zip (init xs) (tail xs)

maxMinDiff xs = (minD, maxD)
  where minD = minimum . map (uncurry (-)) $ zip (init xs) (tail xs)
        maxD = maxDiff xs

--2.2
type NameSurname = String
type Score = Double

studentsPassed :: [(NameSurname, Score)] -> [NameSurname]
studentsPassed xs = [ ns | (ns, sc) <- xs, sc >= (maxSc/2) ]
  where maxSc = maxScore xs

maxScore xs = maximum [ sc | (ns, sc) <- xs]

-- === EXERCISE 3 ===============================================================
--3.1
isTitleCased :: String -> Bool
isTitleCased = all (isUpperCase) . words

isUpperCase w = (head w) `elem` ['A'..'Z']

--3.2
sortPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortPairs = sortBy (comparing snd)

--3.3
filename :: String -> String
filename = reverse . takeWhile (/='/') . reverse

-- === EXERCISE 4 ===============================================================
--4.1
elem' x = foldr (\y acc -> x==y || acc) False

-- === * LECTURE 9 * ===============================================================

-- === EXERCISE 1 ===============================================================
--1.1
type Day = Integer
type Month = Integer
type Year = Integer
data Date = Date Day Month Year deriving(Show)

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

--1.2
data Point = Point Double Double 
   deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
   deriving Show

--1.4
type Manufacturer = String
type Horsepower = Double

data Vehicle = Car Manufacturer Horsepower
             | Truck Manufacturer Horsepower
             | Motorcycle Manufacturer Horsepower
             | Bicycle

-- === EXERCISE 2 ===============================================================
data Level    = Bachelor | Master | PhD deriving (Show, Eq)
data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving Show

newStudent2 = Student "Petar" "Perić" "00364542345" Master 3.5
bestStudent = Student 
  { studentId = "0036491215"
  , firstName = "John", lastName = "Doe"
  , level = Master, avgGrade = 5.0 }  

showStudent :: Student -> String
showStudent s = studentId s ++ " " ++ firstName s ++ " " ++ lastName s

--2.1
improveStudent :: Student -> Student
improveStudent s = s {
    avgGrade = if avgGrade s + 1.0 >= 5.0
                 then 5.0
                 else avgGrade s + 1.0 }

--2.2
avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels students = (avgGradeForLevel Bachelor students,
                              avgGradeForLevel Master students,
                              avgGradeForLevel PhD students)

avgGradeForLevel l xs = if noOfStudents == 0 then 0 else (sum avgGrades) / noOfStudents
  where students     = studentsWithLevel l xs
        avgGrades    = map avgGrade students
        noOfStudents = fromIntegral $ (length students)

studentsWithLevel l = filter (\s -> level s == l)


-- === EXERCISE 3 ===============================================================

--3.1
data MyTriplet = MyTriplet
 { elem1 :: Integer
  ,elem2 :: Double
  ,elem3 :: String }

toTriplet :: MyTriplet -> (Integer, Double, String)
toTriplet t = (elem1 t, elem2 t, elem3 t)

--3.2
data Employee = Employee
  { salary :: Maybe Double
  , name :: String } deriving Show

e1 = Employee { salary = Just $ 1000.0, name = "Anna"}
e2 = Employee { salary = Just $ 2000.0, name = "John"}
e3 = Employee { salary = Just $ 3000.0, name = "Martha"}
e4 = Employee { salary = Nothing, name = "Bertha"}

totalSalaries :: [Employee] -> Double
totalSalaries employees = sum $ (map (getSalary) $ (filter (\e -> salary e /= Nothing ) employees))
 
getSalary e = case salary e of
   Nothing -> 0
   Just n  -> n
