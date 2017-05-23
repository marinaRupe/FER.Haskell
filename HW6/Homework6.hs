import CSVUtils
import Data.List

--2.
type Decision = String

--cases:
caseA = ["Raining", "Yes", "Workday"]
caseB = ["Sunny", "No", "Workday"]

--possible decisions:
c0 = "No"
c1 = "Yes"

--solution:
nbDecide :: CSV -> [String] -> Decision
nbDecide csv caseX = argMax [(calcArg caseX c0 dataset, c0), (calcArg caseX c1 dataset, c1)]
  where dataset = tail $ csv

nbDecideAll :: CSV -> [[String]] -> [Decision]
nbDecideAll csv cases = [ nbDecide csv caseX | caseX <- cases]

--use this CSV or "main" functions to try out solution
dataset :: CSV
dataset = [["Weather","Busy","Week Period","Study Haskell?"],["Sunny","Yes","Weekend","No"],["Raining","No","Weekend","Yes"],["Sunny","No","Workday","Yes"],["Cloudy","Yes","Workday","No"],["Sunny","No","Weekend","No"],["Raining","No","Workday","Yes"],["Cloudy","Yes","Weekend","Yes"]]

--main functions:
mainNbAll :: IO [Decision]
mainNbAll = do
    csv <- readCSV ";" "data.csv"
    let decisions = nbDecideAll csv [caseA, caseB]
    return decisions

mainNb :: [String] -> IO Decision
mainNb caseX = do
    csv <- readCSV ";" "data.csv"
    let decision = nbDecide csv caseX
    return decision

--helper functions:

--number of rows in data
nRows dataset = fromIntegral $ length $ tail dataset

--number of occurrences of specific decision in data
noOfDec dec dataset = fromIntegral $ length $ filter (==dec) (colFields lastColIndex dataset)
  where lastColIndex = (length $ head $ dataset) - 1

--checks if a row has given value in specific column 
hasInCol n val row = val `elem` (colFields n [row])

--checks if a row contains specific decision
hasDecision dec xs = hasInCol lastColIndex dec xs
  where lastColIndex = (length xs) - 1

--probability of specific decision in dataset
probC dec dataset = (noOfDec dec dataset) / (nRows dataset)

--calculate conditional probability for specific decision and given value
calcP (val, col) dec xss = length $ [ xs | xs <- xss, hasInCol col val xs, hasDecision dec xs]

--calculate all conditional probabilities for specific decision
calcProbs caseX dec xss = [ (fromIntegral $ calcP (val, col) dec xss) / (noOfDec dec xss) | (val, col) <- zip caseX [0..]]

--calculate argument for specific decision
calcArg caseX dec dataset = product ([probC dec dataset] ++ calcProbs caseX dec dataset)

--get maximum argument (the one with maximum value)
argMax args = snd $ (maximum args)

--3.
class Truthy a where
    truey :: a -> Bool
    falsey :: a -> Bool

    truey a = not $ falsey a
    falsey a = not $ truey a

instance Truthy Int where
    truey 0 = False
    truey _ = True

instance Truthy Bool where
    truey True = True
    truey False = False

instance Truthy [a] where
    truey [] = False
    truey _ = True
 
--3.a)
if' :: Truthy p => p -> a -> a -> a
if' exp op1 op2 = if truey exp then op1 else op2

--3.b)
assert :: Truthy p => p -> a -> a
assert exp1 exp2 = if truey exp1 then exp2 else error "Assertion failed"

--3.c)
(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
x &&& y = (truey x) && (truey y)

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
x ||| y = (truey x) || (truey y)


--4.a)
data DiffList a  = DiffList
    { undiff :: [a] } deriving (Show)


concatOp x y = [1,2]++[3]++y
dl x = DiffList (concatOp ([1,2]++[3]) x)
dl2 = DiffList ([1,2]++[3])

--4.b)
empty :: DiffList a
empty = DiffList []

--4.c)
--fromList :: [a] -> DiffList a
--fromList l = DiffList 

--5.
data MyList a = Cons a (MyList a) | Nil deriving Show

append (Cons x xs) exp2 = Cons x (append xs exp2)
append Nil exp2         = exp2

--5.a)
instance Functor MyList where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    fmap f (Nil)       = Nil

--5.b)
instance Applicative MyList where
    pure x = Cons x Nil
    (Cons f Nil) <*> exp = (fmap f exp)
    (Cons f fs) <*> exp  = (fmap f exp) `append` (fs <*> exp)
    _ <*> Nil            = Nil

--5.c)
instance Monad MyList where
    (Cons x Nil) >>= f = f x
    (Cons x xs) >>= f  = (f x) `append` (xs >>= f)
    Nil >>= f          = Nil

fun ::  Int -> MyList Int
fun x = Cons (x + 1) (Cons (x * 2) Nil)

