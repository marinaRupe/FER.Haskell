import Data.Char
import Data.List

--Lecture 1

-- === EXERCISE 1 ==============================================================

--1.1
concat3 s1 s2 s3 =
    s1 ++ (if length s2 >= 2 then s2 else "") ++ s3

--1.2
showSalary amount bonus =
    "Salary is " ++ show amount ++ (if bonus /= 0 then ", and a bonus " ++ show bonus else "")


--Lecture 2

-- === EXERCISE 1 ==============================================================

--1.1
--my solution:
removeLastThree l = take (length l - 3) l
removeFirstThree l = tail (tail (tail l))
removeThree l = removeLastThree $ removeFirstThree l

--better solution
removeThree' l = reverse $ drop 3 $ reverse $ drop 3 l

--1.2
initials s1 s2 = [head s1] ++ "." ++ [head s2] ++ "."

--1.3
concat2 s1 s2 =
    if length s1 >= length s2 then s1 ++ s2 else s2 ++ s1

--1.4
safeHead l =
    if null l then [] else [head l]

--1.5
hasDuplicates l =
    length l > (length $ nub l)


-- === EXERCISE 2 ==============================================================

--2.1
doublesFromTo a b
    | b < a = doublesFromTo b a
    | otherwise = [x*2 | x <- [a..b]]

--2.2
caesarCode n xs =
    [chr( ( ord(toLower (c)) - ord('a') + n) `mod` 26 + ord('a') ) | c <- xs, c /= ' ']


-- === EXERCISE 3 ==============================================================

--3.1
letterCount s =
    sum $ [length w | w <- words (s), length w >= 3]

--3.2

--first solution
isPalindrome s = [toLower c | c <- s, c /= ' '] == [toLower c | c <- reverse s, c /= ' ']

--second solution (with helper function)
lowerCaseWithoutBlanks s = [toLower c | c <- s, c /= ' ']
isPalindrome' s = (lowerCaseWithoutBlanks s) == (lowerCaseWithoutBlanks $ reverse s)

--3.3
flipp xss = concat $ reverse $ [reverse xs | xs <- xss]


-- === EXERCISE 4 ==============================================================

--4.1
abs n = if n < 0 then -n else n
inCircle r x y = [ (a,b) | a <- [-10..10], b <- [-10..10], (a-x)^2 + (b-y)^2 <= r^2 ]

--rg - resolution of the grid
inCircle' r x y rg = [ (a,b) | a <- [-rg..rg], b <- [-rg..rg], (a-x)^2 + (b-y)^2 <= r^2 ]


--4.2
steps xs = zip xs (tail xs)


-- === EXERCISE 5 ==============================================================

--5.1
indices x xs = [ fst ix | ix <- zip [0..] xs, snd ix == x]

--5.2
indexLines line = zip [1..] line
showLineNumbers s = [indexedLine | indexedLine <- indexLines $ lines s]

--5.3
haveAlignment xs ys = if null (common xs ys) then False else True
common xs ys = [ fst t | t <- zip xs ys, fst t == snd t ]