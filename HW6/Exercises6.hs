import Control.Monad
import Data.List

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person = Person {
   idNumber :: String,
   forename :: String,
   surname  :: String,
   sex      :: Sex,
   age      :: Int,
   partner  :: Maybe Person,
   children :: [Person] } deriving (Read,Eq,Ord)

data Person2 = Person2 {
   personId2 :: String,
   forename2 :: String,
   surname2  :: String,
   sex2      :: Sex,
   mother2   :: Maybe Person2,
   father2   :: Maybe Person2,
   partner2  :: Maybe Person2,
   children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

-- === EXERCISE 1 ===============================================================

--1.1
foo = foo

--1.2
parentCheck :: Person2 -> Bool
parentCheck p = p `elem` (fathersChildren p ++ mothersChildren p)
  where fathersChildren :: Person2 -> [Person2]
        fathersChildren p = case father2 p of 
            Just f -> children2 f
            Nothing -> []
        mothersChildren :: Person2 -> [Person2]
        mothersChildren = maybe [] (children2) . mother2


-- === EXERCISE 2 ===============================================================
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)

--2.1
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

--2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)


-- === EXERCISE 3 ===============================================================
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

--3.1
treeMax :: Ord a => Tree a -> a
treeMax Null = error "Empty tree"
treeMax (Node x Null Null) = x
treeMax (Node x Null rb) = x `max` treeMax rb
treeMax (Node x lb Null) = x `max` treeMax lb
treeMax (Node x lb rb) = x `max` treeMax lb `max` treeMax rb


-- === EXERCISE 4 ===============================================================

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

--4.1
listToTree :: Ord a => [a] -> Tree a
listToTree [] = Null
listToTree xs = listToTree' xs Null
  where listToTree' [] t = t
        listToTree' (x:xs) t = listToTree' xs $ treeInsert x t


-- === EXERCISE 5 ===============================================================

data Weekday = 
   Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
   deriving (Show,Enum)

--5.1
instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False


--5.2
pero  = Person "2323" "Pero"  "Perić" Male   45 (Just ana)   [marko]
ana   = Person "3244" "Ana"   "Anić"  Female 43 (Just pero)  [marko,iva]
marko = Person "4341" "Marko" "Perić" Male   22 (Just maja)  []
maja  = Person "7420" "Maja"  "Majić" Female 20 (Just marko) []
iva   = Person "4642" "Iva"   "Ivić"  Female 16 Nothing      []

instance Show Person where
    show p = "Person {idNumber = " ++ (show $ idNumber p)
            ++ ", forename = " ++ (forename p)
            ++ ", surname = " ++ (surname p)
            ++ ", sex = " ++ (show $ sex p)
            ++ ", age = " ++ (show $ age p)
            ++ ", partner = " ++ case partner p of 
                Just par -> forename $ par
                Nothing -> ""
            -- ++ ", children = " ++ show $ (map (forename) (children p))
            ++"}"


-- == EXERCISE 6 ================================================================

--6.1
instance Eq a => Eq (MyList a) where
    Empty == Empty = True
    _  == Empty = False
    Empty == _     = False
    (Cons x _) == (Cons y _) = x == y 


-- -------------------LECTURE 11------------------------------------

-- === EXERCISE 1 ===============================================================

--1.1
main = do
    putStrLn "Enter the first string: "
    s1 <- getLine
    putStrLn "Enter the second string: "
    s2 <- getLine
    putStrLn . reverse $ s1 ++ s2

--1.2
threeNumbers = do
    putStrLn "Enter three numbers: "
    n1 <- getLine
    n2 <- getLine
    n3 <- getLine
    print $ read n1 + read n2 + read n3

-- === EXERCISE 2 ===============================================================

--2.1
threeStrings = do
    s1 <- getLine
    s2 <- getLine
    s3 <- getLine
    do
        let str = s1 ++ s2 ++ s3
        putStrLn str
        return $ length str

--2.2
askNumber9 = do
    n <- getLine
    if hasOnlyDigits n
        then return (read n) :: IO Int
        else askNumber9

digits = ['0'..'9']
hasOnlyDigits xs = not $ False `elem` [ x `elem` digits | x <- xs ]

main2 = do
    number <- askNumber9
    putStrLn $ show number

--2.3
askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
    putStrLn m
    s <- getLine
    if p s
        then askUser m p
        else return s
    
main3 = askUser

--2.4
inputStrings :: IO [String]
inputStrings = do
    let strings = []
    getStr strings

getStr strings = do
    s <- getLine
    if s == ""
        then return strings
        else getStr (strings ++ [s])
