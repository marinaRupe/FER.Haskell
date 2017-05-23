-- == EXERCISE 1 ================================================================

--1.1

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

--1.2

headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf (x:xs) = head x : headsOf xs

addToList :: Num a => a -> [a] -> [a]
addToList _ []     = []
addToList n (x:xs) = x + n : addToList n xs

incIncList' :: Num a => [a] -> [a]
incIncList' xs = inc 0 xs
  where inc _ [] = []
        inc n (x:xs) = x + n : inc (n+1) xs


-- == EXERCISE 2 ================================================================

--2.1

modMult n m xs = multWithMod xs
  where mod' = n `mod` m
        multWithMod [] = []
        multWithMod (x:xs) = (x * mod') : multWithMod xs


--2.2

addPredecessor :: Num a => [a] -> [a]
addPredecessor [] = []
addPredecessor xs = step 0 xs
  where
    step _ [] = []
    step p (x:xs) = x + p : step x xs


numPositives :: (Num a, Ord a) => [a] -> Int
numPositives [] = 0
numPositives (x:xs) | x > 0     = 1 + numPositives xs
                    | otherwise = numPositives xs

-- == EXERCISE 3 ================================================================

--3.1

equalTriplets [] = []
equalTriplets ((a,b,c):xs)
  | a==b && a==c && b==c = (a,b,c) : equalTriplets xs
  | otherwise = equalTriplets xs

--3.2

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n [x]    = x : take' (n-1) [x]
take' n (x:xs) = x : take' (n-1) xs


-- == EXERCISE 4 ================================================================

--4.1

drop' :: Int -> [a] -> [a]
drop' 1 (x:xs) = xs
drop' _ []     = []
drop' n [x]    = drop' (n-1) [x]
drop' n (x:xs) = drop' (n-1) xs

--4.2
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ []     = []
takeFromTo 0 0 (x:xs) = [x]
takeFromTo 0 b (x:xs) = x : takeFromTo 0 (b-1) xs
takeFromTo a b (x:xs) = takeFromTo (a-1) (b-1) xs -- if b > length xs - just stops at the end of xs


-- == EXERCISE 5 ================================================================

--5.1

eachThird :: [a] -> [a]
eachThird [] = []
eachThird [x] = []
eachThird (x:y:[]) = []
eachThird (x:y:z:xs) = z : eachThird xs


-- == EXERCISE 6 ================================================================

--6.1
length' :: [a] -> Int
length' xs = len' xs 0
  where len' [] n     = n
        len' (_:xs) n = len' xs (n+1) 

