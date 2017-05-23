import Data.List
import Data.Char

--1.
intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs (ys:[])  = ys
intercalate' xs (ys:yss) = ys ++ xs ++ (intercalate' xs yss)

--2.

--2.a)
chunk :: Int -> [a] -> [[a]]
chunk 0 _  = []
chunk n xs = if length xs <= n
    then [xs]
    else sublist : (chunk n remaining)
  where (sublist, remaining) = splitAt n xs


--2.b)
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy []     _  = []
chunkBy _      [] = [] 
chunkBy (x:xs) ys = if x > 0
    then sublist : (chunkBy xs remaining)
    else chunkBy xs ys
  where (sublist, remaining) = splitAt x ys


--2.c)
chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 xs = []
chunkInto n xs
    | length xs < n = chunk 1 xs
    | modResult == 0 = chunk divResult xs
    | otherwise      = addToLastElement remainder (chunk divResult xsWithoutRemainder)
  where modResult = length xs `mod` n
        divResult = length xs `div` n
        xsWithoutRemainder = take (length xs - modResult) xs
        remainder = take modResult xs

addToLastElement ys xss = init xss ++ [(last xss) ++ ys]


--3.
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _  = [] 
cycleMap fs xs = [ (getIthElement (i `mod` length fs) fs) x | (i, x) <- index xs]

index xs = zip [0..] xs
getIthElement i xs = head $ [ x | (j, x) <- index xs, j == i ]



--4.a)
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce f x []     = x
reduce f x (y:ys) = reduce f (f x y) ys


--4.b)
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ []       = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs


--4.c)
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ x []     = [x]
scan f x (y:ys) = sc f [x, f x y] ys
  where sc _ xs []     = xs
        sc f xs (y:ys) = sc f (xs ++ [f (last xs) y]) ys


--4.d)
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f x ys = rred f x (reverse ys)
  where rred f x []     = x
        rred f x (y:ys) = rred f (f y x) ys


--4.e)
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 f []     = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) (init xs)


--4.f)
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ x []     = [x]
rscan f x ys = rsc f [f (last ys) x, x] (tail $ reverse ys)
  where rsc _ xs []     = xs
        rsc f xs (y:ys) = rsc f ([f y (head xs)] ++ xs) ys


--5.a)
type Tolerance = Double
newton :: Tolerance -> Double -> Double
newton tol x = if x < 0 then error "can't get sqrt of negative number"
    else newton' tol x (initialGuess x)

initialGuess x = x / 2 + 1

newton' tol x lastApprox = if abs (y' - lastApprox) > tol
    then newton' tol x y'
    else y'
  where y' = (lastApprox + x / lastApprox) / 2


--5.b)
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx

dx = 0.00001


--8.
permutations' :: [a] -> [[a]]
permutations' []     = []
permutations' [x]    = [[x]]
permutations' [x, y] = [[x]++[y], [y]++[x]]
permutations' (x:xs) = permElement x (permutations' xs)

permElement elem xss = [ insertElementOnIndex elem n xs | xs <- xss, n <- [0..length xs] ]

insertElementOnIndex elem n xs = ys ++ [elem] ++ zs 
  where (ys,zs) = splitAt n xs
