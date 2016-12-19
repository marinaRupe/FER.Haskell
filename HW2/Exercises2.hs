import Data.Char
import Data.List

-- === EXERCISE 1 ===============================================================
--1.1
headHunter :: [[a]] -> a
headHunter xss@([]:[]:(x:_):_) = x
headHunter xss@([]:(x:_):_) = x
headHunter xss@((x:_):_) = x
headHunter _ = error "First three lists don't have a head!"

--1.2
fistColumn' m = [ head xs | xs <- m] --crashes if the list is empty
fistColumn m = [ x | (x:_) <- m]

--1.3
shoutOutLoud :: String -> String
shoutOutLoud msg = unwords $ [ h:h:h:t | (h:t) <- words msg]
shoutOutLoud' msg = unwords $ [ (take 100 $ repeat h) ++ t | (h:t) <- words msg]


-- === EXERCISE 2 ===============================================================
--2.1
pad :: String -> String -> (String, String)
pad (x:xs) (y:ys) = ( (toUpper x) : adjust len xs, (toUpper y) : adjust len ys)
  where len = max (length xs) (length ys)
        adjust n s = s ++ replicate (n - length s) ' '
        

-- === EXERCISE 3 ===============================================================
--3.1
pad' :: String -> String -> (String, String)
pad' (x:xs) (y:ys) = let
               s1 = toUpper x : xs ++ (replicate (len - length xs) ' ')
               s2 = toUpper y : ys ++ (replicate (len - length ys) ' ')
               len = max (length xs) (length ys) in 
                   (s1, s2)

-- === EXERCISE 4 ===============================================================
--4.1
ones (a,b) (_:xs) = "The pair " ++ onesInPair (a,b) ++ " and the second element of the list is " ++ second
   where second = show $ head xs

onesInPair (a, b)
    | (a,b) == (1,1)                    = "contains two ones" 
    | fst (a,b) == 1 || snd (a,b) == 1  = "contains one one"
    | otherwise                         = "does not contain a single one"
