import Data.List

--1.
type Position = (Integer, Integer)
data Orientation = West | East | North | South deriving (Eq, Show)
data TurnDir = CW | CCW deriving (Eq, Show)

--1.a)
data Turtle = Turtle { pos    :: Position
                     , orient :: Orientation } deriving Show

--1.b)
position :: Turtle -> Position
position = pos

--1.c)
orientation :: Turtle -> Orientation
orientation = orient

--1.d)
newTurtle = Turtle { pos = (0,0)
                   , orient = North}
leonardo = newTurtle

--1.e)
move :: Integer -> Turtle -> Turtle
move d turtle
  | d < 0           = error "Turtles cannot move backwards"
  | orient == West  = turtle { pos = (x - d, y) }
  | orient == East  = turtle { pos = (x + d, y) }
  | orient == North = turtle { pos = (x, y + d) }
  | orient == South = turtle { pos = (x, y - d) }
    where orient = orientation turtle
          x      = fst $ position turtle
          y      = snd $ position turtle

--1.f)
turn :: TurnDir -> Turtle -> Turtle
turn turndir turtle
  | (currentOrient == West)  && (turndir == CW)   = turtle { orient = North }
  | (currentOrient == West)  && (turndir == CCW)  = turtle { orient = South }
  | (currentOrient == East)  && (turndir == CW)   = turtle { orient = South }
  | (currentOrient == East)  && (turndir == CCW)  = turtle { orient = North }
  | (currentOrient == North) && (turndir == CW)   = turtle { orient = East }
  | (currentOrient == North) && (turndir == CCW)  = turtle { orient = West }
  | (currentOrient == South) && (turndir == CW)   = turtle { orient = West }
  | (currentOrient == South) && (turndir == CCW)  = turtle { orient = East }
    where currentOrient = orientation turtle

--1.g)
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle []     t = t
runTurtle [f]    t = f t
runTurtle (f:fs) t = runTurtle fs (f t)

spiral i = move i : turn CW : spiral(i + 1)


--4.a)
sortTracks :: [String] -> [String]
sortTracks = map (snd) . sort . map (\x -> (trackNo x, x))

trackNo = filter (isTrackNo) . words
isTrackNo xs = and [isDigit x | x <- xs]
isDigit = (`elem` ['0'..'9'])

--4.b)
numberOfPlays :: [String] -> Integer
numberOfPlays xs = foldr (+) 0 (map (read . head . words) xs)


--5.
possibleDirections = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"]

possibleRoutes :: String -> Int
possibleRoutes xs = calcRoutes 1 xs

calcRoutes n [] = n
calcRoutes n [x] = if [x] `elem` possibleDirections
  then n
  else error "Typo in a message!"
calcRoutes n (x:y:xs)
  | ([x] ++ [y]) `elem` possibleDirections = calcRoutes (n*2) (y:xs)
  | [x] `elem` possibleDirections          = calcRoutes n (y:xs)
  | otherwise                              = error "Typo in a message!"

