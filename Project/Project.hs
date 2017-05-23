import Data.List
import Data.Char
import Text.Read

data Exp = Value VarOrConst | Operator Op (Exp) (Exp) | Function Fun (Exp)
data VarOrConst = Var String | Const Double deriving (Show)
data Op = Plus | Minus | Times | Divide | Power deriving (Show, Eq)
data Fun = Sin | Cos | Exp | Log deriving (Show)

instance Show Exp where
    show (Value (Var a))             = " " ++ a ++ " "
    show (Value (Const a))           = " " ++ show a ++ " "
    show (Function Sin exp1)         = " sin" ++ show exp1
    show (Function Cos exp1)         = " cos" ++ show exp1
    show (Function Exp exp1)         = " exp" ++ show exp1
    show (Function Log exp1)         = " log" ++ show exp1
    show (Operator Plus exp1 exp2)   = " (" ++ (show exp1) ++ "+" ++ (show exp2) ++ ") "
    show (Operator Minus exp1 exp2)  = " (" ++ (show exp1) ++ "-" ++ (show exp2) ++ ") "
    show (Operator Times exp1 exp2)  = " (" ++ (show exp1) ++ "*" ++ (show exp2) ++ ") "
    show (Operator Divide exp1 exp2) = " (" ++ (show exp1) ++ "/" ++ (show exp2) ++ ") "
    show (Operator Power exp1 exp2)  = " (" ++ (show exp1) ++ "^" ++ (show exp2) ++ ") "

--tests
expr1 = "2 * x + 3 * y ^ 2 + cos y"
expr2 = "x * 2 + 3 * ( y + 1 )"
expr3 = "5 * 2 + 3 * ( 3 + 1 )"
expr4 = "x * 2.4 + 3 * ( y + 1 )"
expr5 = "cos x + sin x"
expr6 = "x + x ^ 2 + x ^ 3 + x ^ 4"
expr7 = "4 * x + ( 5 * y ^ 2  + 6 * z )"

--create expression
createExpression :: String -> Exp
createExpression input = getTree (infixToPostfix [] (words input)) []

getTree :: [[Char]] -> [Exp] -> Exp
getTree [] [exp] = exp

getTree (x:xs) (y:z:ys)
    | x == "+" = getTree xs ( (Operator Plus (z) (y)) : ys )
    | x == "-" = getTree xs ( (Operator Minus (z) (y)) : ys )
    | x == "*" = getTree xs ( (Operator Times (z) (y)) : ys )
    | x == "/" = getTree xs ( (Operator Divide (z) (y)) : ys )
    | x == "^" = getTree xs ( (Operator Power (z) (y)) : ys )
getTree (x:xs) (y:ys)

    | x == "sin" = getTree xs ( (Function Sin (y)) : ys )
    | x == "cos" = getTree xs ( (Function Cos (y)) : ys )
    | x == "exp" = getTree xs ( (Function Exp (y)) : ys )
    | x == "log" = getTree xs ( (Function Log (y)) : ys )

getTree (x:xs) stack
    | isNumber' x = getTree xs ( (Value (Const (read x :: Double))) : stack )
    | otherwise   = getTree xs ( (Value (Var x)) : stack )


--substitute variables with constants
substitute :: String -> Exp -> Double -> Exp
substitute varName exp@(Value (Var a)) varValue     = if varName == a then Value (Const varValue) else exp
substitute varName exp@(Value (Const a)) varValue   = exp
substitute varName (Operator op exp1 exp2) varValue = Operator op (substitute varName exp1 varValue) (substitute varName exp2 varValue)
substitute varName (Function f exp) varValue        = Function f (substitute varName exp varValue)

--calculate expression
evaluate :: Exp -> Double
evaluate (Value (Var a))             = error "Tree still contains variables, substitute them before evaluation"
evaluate (Value (Const a))           = a
evaluate (Function Sin exp1)         = sin $ evaluate exp1
evaluate (Function Cos exp1)         = cos $ evaluate exp1
evaluate (Function Exp exp1)         = exp $ evaluate exp1
evaluate (Function Log exp1)         = log $ evaluate exp1
evaluate (Operator Plus exp1 exp2)   = (+) (evaluate exp1) (evaluate exp2)
evaluate (Operator Minus exp1 exp2)  = (-) (evaluate exp1) (evaluate exp2)
evaluate (Operator Times exp1 exp2)  = (*) (evaluate exp1) (evaluate exp2)
evaluate (Operator Divide exp1 exp2) = (/) (evaluate exp1) (evaluate exp2)
evaluate (Operator Power exp1 exp2)  = (**) (evaluate exp1) (evaluate exp2)

isNumber' s = isDouble (readMaybe s :: Maybe Double)
isDouble (Just a) = True
isDouble Nothing  = False 

--infix to postfix (Shunting-yard algorithm)
infixToPostfix :: [String] -> [String] -> [String]
infixToPostfix (s:ss) [] = s:(infixToPostfix ss [])
infixToPostfix [] []     = []
infixToPostfix stack@(s:ss) (x:xs)
  | (all isAlphaNumOrDot x) && (not $ isFunction x) = x:(infixToPostfix stack xs)
  | isFunction x                                    = infixToPostfix (x:stack) xs
  | isOperator x && isLeft x && precLessEq x s      = s:(infixToPostfix ss (x:xs))
  | isOperator x && isRight x && precLess x s       = s:(infixToPostfix ss (x:xs))
  | isOperator x && (not $ precLessEq x s)          = infixToPostfix (x:stack) xs
  | x == "("                                        = infixToPostfix (x:stack) xs
  | x == ")" = if s == "("
               then (if isFunction $ head ss
                    then (head ss):(infixToPostfix (tail ss) xs)
                    else infixToPostfix ss xs)
               else s:(infixToPostfix ss (x:xs))
 
infixToPostfix [] (x:xs)
  | all isAlphaNumOrDot x && (not $ isFunction x)  = x:(infixToPostfix [] xs)
  | isFunction x                                   = infixToPostfix [x] xs
  | isOperator x                                   = infixToPostfix [x] xs


op2 = ["+", "-"]
op3 = ["*", "/"]
operators = op2 ++ op3 ++ ["^"]

isAlphaNumOrDot x = isAlphaNum x || x == '.'
isOperator x      = x `elem` operators
isFunction x      = x `elem` ["sin", "cos", "exp", "log"]
isLeft x          = x `elem` (op2 ++ op3)
isRight x         = x == "^"
 
precLess x y
  | x `elem` op2 = y `elem` op3 || y == "^"
  | x `elem` op3 = y == "^"
  | otherwise  = False
 
precLessEq x y
  | x `elem` op2 = y `elem` op2 || y `elem` op3 || y == "^"
  | x `elem` op3 = y `elem` op3 || y == "^"
  | otherwise  = False