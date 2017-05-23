import Data.Char
import Data.List

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

-- === EXERCISE 2 ================================================================

--2.1
applyOnLast f xs ys = f (last xs) (last ys)

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast (addThree 100)

--2.2
applyManyTimes n foo x = apply (replicate (n-1) foo) (foo x)
  where apply [] result     = result
        apply (f:fs) result = apply fs (f result)


finishSentence :: String -> String
finishSentence = (++ ".")

-- === EXERCISE 3 ================================================================

--3.1
listifylist :: [a] -> [[a]]
listifylist = map (:[])

--3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (checkValue n)

checkValue n x = if x > n then n else x

-- === EXERCISE 4 ================================================================

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