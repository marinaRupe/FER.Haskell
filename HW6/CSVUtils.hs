module CSVUtils
    ( parseCSV
    , showCSV
    , colFields
    , readCSV
    , writeCSV
    , Separator
    , Document
    , CSV 
    , Entry
    , Field
    ) where

import System.IO
import Data.List.Split
import Data.List

-- 1.
type Separator = String
type Document  = String
type CSV       = [Entry]
type Entry     = [Field]
type Field     = String

doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"

--1.a)
parseCSV :: Separator -> Document -> CSV
parseCSV s = map (splitOn s) . lines


--1.b)
csv = parseCSV ";" doc

showCSV :: Separator -> CSV -> Document
showCSV s = unlines . map (intercalate s)

--1.c)
colFields :: Int -> CSV -> [Field]
colFields i csv = if null $ result
    then error $ "There is no column " ++ show i ++ " in the CSV document"
    else result
  where result = map (snd) $ concat $ map (checkColumn i . zip [0..]) csv

checkColumn i  = filter (\(x,y) -> x == i)

--1.d)
readCSV :: Separator -> FilePath -> IO CSV
readCSV s f = do
    doc <- readFile f
    return $ parseCSV s doc

--1.e)
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV s f cvs = do
    let doc = showCSV s cvs
    writeFile f doc

--1.f) done at the beginning of this file