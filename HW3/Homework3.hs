import Data.Char

--1.a)
type RomanNumeral = String

isValidRoman :: RomanNumeral -> Bool
isValidRoman xs = contRomanLiterals numeral && checkPositions numeral && rule1 numeral && rule4 numeral
  where numeral = toUpperCase xs


--Roman literals
romanLiterals = ['I', 'V', 'X', 'L', 'C', 'D', 'M']
allowedSubtrDig = ['I', 'X', 'C']
beginningWith5 = ['V', 'L', 'D']
notBeginningWith5 = allowedSubtrDig ++ ['M']


--helper functions
toUpperCase s = [ toUpper c | c <- s]
index xs = zip [1..] xs


--check whether the given numeral contains only Roman literals
contRomanLiterals xs = not (False `elem` [ isValidRomanLiteral c | c <- xs])

isValidRomanLiteral c
    | (toUpper c) `elem` romanLiterals = True
    | (toUpper c) == 'N'               = error "Zero cannot be represented with N"
    | otherwise                        = False


--check if all positions are OK
checkPositions [x] = True
checkPositions (x:y:[]) = chkPos x y
checkPositions (x:y:xs) = if chkPos x y
  then checkPositions (y:xs)
  else False

chkPos n m
  | i >= j = True
  | (i+1) == j && n `elem` allowedSubtrDig = True
  | otherwise = False
    where i = getPosition n
          j = getPosition m

getPosition n = if n `elem` notBeginningWith5
  then head $ [ i | (i, x) <- index notBeginningWith5, x == n]
  else head $ [ i | (i, x) <- index beginningWith5, x == n ]


--check occurrences
rule1 [x] = True
rule1 (x:y:[]) = True
rule1 (x:y:z:[]) = True
rule1 (x:y:z:k:[]) = chkRule1 x y z k
rule1 (x:y:z:k:xs) = if chkRule1 x y z k
  then rule1 (y:z:k:xs)
  else False

chkRule1 x y z k
  | x == y && y == z && z == k = False
  | otherwise = True

  
rule4 xs = not ( False `elem` [ count x xs <= 1 | x <- beginningWith5 ] )

count y [] = 0
count y (x:xs) = if x == y
  then 1 + count y xs
  else count y xs

--1.b)
toRoman :: Int -> RomanNumeral
toRoman n = if n >= 1 && n <= 3999
    then convertNumberToRoman n
    else error "Number cannot be represented"

--returns list of digits starting with digit on the lowest position in the number
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

convertNumberToRoman n = concat $ reverse $ [ convertDigitToRoman d | d <- index $ digits n ]

convertDigitToRoman (position, digit)
    | digit == 0    =  ""
    | digit <= 3    =  convertDigitFrom1To3 (position, digit)
    | digit == 4    =  convertDigit4 position
    | digit == 5    =  convertDigit5 position
    | digit <= 8    =  convertDigitFrom6To8 (position, digit-5)
    | otherwise     =  convertDigit9 position

convertDigitFrom1To3 (position, timesRepeated) = concat $ replicate timesRepeated [ c | (p, c) <- index notBeginningWith5, p == position ]

convertDigit4 position = concat $ [ [c1] ++ [c2] | (p1, c1) <- index allowedSubtrDig, (p2, c2) <- index beginningWith5, p1 == position, p2 == position ]

convertDigit5 position = [ c | (p, c) <- index beginningWith5, p == position ]

convertDigitFrom6To8 (position, timesRepeated) = concat $ [ [c2] ++ convertDigitFrom1To3 (position, timesRepeated) | (p1, c1) <- index notBeginningWith5, (p2, c2) <- index beginningWith5, p1 == position, p2 == position ]

convertDigit9 position = concat $ [ [c1] ++ [c2] | (p1, c1) <- index notBeginningWith5, (p2, c2) <- index notBeginningWith5, p1 == position, p2 == (position+1)]


--2.

shortestDistance :: (Int, Int) -> [(Int, Int)] -> Int
shortestDistance _ [] = 0
shortestDistance home pubs = shortestDist home home pubs 0

shortestDist home current [] dist = dist + (manhattanDistance home current)
shortestDist home current remaining dist =  shortestDist home newCurrent newRemaining (dist+shortest)
  where distances = distancesFrom current remaining
        shortest = minimum $ distances
        newCurrent = head $ [ place | (i, place) <- index remaining, (j, d) <- index distances, i == j, d == shortest ]
        newRemaining = [ r | r <- remaining, r /= newCurrent]

        
manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1,x2) (y1,y2) = ( abs $ (x1-y1) ) + ( abs $ (x2-y2) )


distancesFrom xs yss = [ manhattanDistance xs ys | ys <- yss ]


--3.a)

type Probability = Double
type DiscreteRandVar = [(Int, Probability)]

x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]


mean  :: DiscreteRandVar -> Double
mean []     = 0
mean (xs:xss) = xi * pi + mean xss
    where xi = fromIntegral $ fst xs
          pi = snd xs


mean' :: DiscreteRandVar -> Double
mean' xss = mean'' xss 0

mean'' :: DiscreteRandVar -> Double -> Double
mean'' [] n       = n
mean'' (xs:xss) n = mean'' xss (xi*pi + n)
    where xi = fromIntegral $ fst xs
          pi = snd xs


--3.b)

variance  :: DiscreteRandVar -> Double
variance [] = 0
variance xss = var xss (mean' xss)

var :: DiscreteRandVar -> Double -> Double
var [] qx       = 0
var (xs:xss) qx = (xi - qx)^2 * pi + var xss qx
    where xi = fromIntegral $ fst xs
          pi = snd xs


variance' :: DiscreteRandVar -> Double
variance' xss = var' xss 0 (mean' xss)

var' :: DiscreteRandVar -> Double -> Double -> Double
var' [] n qx       = n
var' (xs:xss) n qx =  var' xss ( (xi - qx)^2 * pi + n ) qx
    where xi = fromIntegral $ fst xs
          pi = snd xs

--3.c)

probabilityFilter  :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter p [] = []
probabilityFilter p (xs:xss) = if pi >= p
    then xi : next
    else next
  where xi = fst xs
        pi = snd xs
        next = probabilityFilter p xss


probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' p xss = reverse' $ probFilt p xss []

probFilt :: Probability -> DiscreteRandVar -> [Int] -> [Int]
probFilt p [] l       = l
probFilt p (xs:xss) l = if pi >= p
    then probFilt p xss (xi:l)
    else probFilt p xss l
  where xi = fst xs
        pi = snd xs

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev []     ys = ys
        rev (x:xs) ys = rev xs (x:ys)
