module Day6
  (
    day6
  , day6Part2
  ) where

import Data.List

-- Part 1
groupByNewLine :: [String] -> String -> [String]
groupByNewLine [] str = [str]
groupByNewLine strs "" = "":strs
groupByNewLine (str:strs) s = nub (str ++ s):strs

day6 :: String -> Int
day6 str = let allLines = lines str
               grouped = foldl groupByNewLine [] allLines
            in sum $ map length grouped
               
-- Part 2
type GroupAnswers = (Int, String, String)
groupByPeople :: [GroupAnswers] -> String -> [GroupAnswers]
groupByPeople [] str = [(1, str, str)]
groupByPeople strs "" = (0, "", ""):strs
groupByPeople ((numPeople, grpStr, indStr):strs) s = if numPeople == 0
                                                     then (numPeople + 1, s, s):strs
                                                     else (numPeople + 1, grpStr ++ s, indStr):strs

countTrues :: [Bool] -> Int
countTrues = foldl (\acc x -> if x then acc + 1 else acc) 0 

countOccurrences :: Int -> String -> Char -> Bool
countOccurrences n str c = let indices = c `elemIndices` str
                            in n == length indices

numberOfAnswers :: GroupAnswers -> Int
numberOfAnswers (n, grpStr, indStr) = countTrues $ map (countOccurrences n grpStr) indStr

day6Part2 :: String -> Int
day6Part2 str = let allLines = lines str
                 in sum $ map numberOfAnswers $ foldl groupByPeople [] allLines