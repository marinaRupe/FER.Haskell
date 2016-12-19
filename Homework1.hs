import Data.Char
import Data.List


--1.a)
norm :: (Double, Double) -> Double
norm (x, y) = sqrt $ (x^2 + y^2)

--1.b)
normalize :: (Double, Double) -> (Double, Double)
normalize (x, y) =
    if norm (x, y) /= 0 then (x / (norm (x, y)), y / (norm (x, y)))
        else error "Cannot normalize null vector"

--1.c)
scalarMult :: (Num a) => a-> (a, a) -> (a, a)
scalarMult a (x, y) = (a*x, a*y)


--1.d)
dot :: (Double, Double) -> (Double, Double) -> Double
dot (x1, y1) (x2, y2) = x1*x2 + y1*y2

--1.e)
cos' :: (Double, Double) -> (Double, Double) -> Double
cos' (x1, y1) (x2, y2) =
    if (x1, y1) == (0, 0) || (x2, y2) == (0, 0) then error "Null vector given"
        else normalize (x1, y1) `dot` normalize (x2, y2)

--1.f)
areParalel :: (Double, Double) -> (Double, Double) -> Bool
areParalel (x1, y1) (x2, y2) = normalize (x1, y1) == normalize (x2, y2)


--2.
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' i l = if (i > length l || i < 0) then error "n is out of range"
    else ( [ snd x | x <- index l, fst x <= i], [ snd x | x <- index l, fst x > i] )


--3.a)
index :: [a] -> [(Int, a)]
index xs = zip [1..] xs

doubleEverySecond :: Num a => [a] -> [a]
doubleEverySecond xs = reverse $ [ if (even $ fst x) then (snd x)*2 else snd x | x <- index $ reverse xs]

--3.b)
hasOneDigit :: Int -> Bool
hasOneDigit n = n `div` 10 == 0

sumDigitsOfANumber :: Int -> Int
sumDigitsOfANumber n = if hasOneDigit n then n else (n `div` 10 + n `mod` 10) --assumption: a number has one or two digits

sumDigits :: [Int] -> Int
sumDigits xs = sum $ [sumDigitsOfANumber x | x <- xs]

--3.c)
remainder :: Int -> Int
remainder n = n `mod` 10

stringToIntList :: String -> [Int]
stringToIntList xs = [ digitToInt x | x <- xs]

--final solution:
luhn :: String -> Bool
luhn xs = (remainder $ sumDigits $ doubleEverySecond $ stringToIntList xs) == 0


--4.
factorize ::  Int -> [Int]
factorize n = [ x | x <- [1..n], n `mod` x == 0]

hasDividents :: Int -> Bool
hasDividents n = True `elem` [ n `mod` x == 0 | x <- [2..n-1] ]

primes :: Int -> [Int]
primes n = take n [ x | x <- [2..], not (hasDividents x) ]