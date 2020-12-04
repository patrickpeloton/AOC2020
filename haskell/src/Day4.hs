module Day4 
  (
    day4
  , day4Part2
  ) where

import Data.List
-- Part 1
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

groupBySpace :: [String] -> String -> [String]
groupBySpace [] curr = [curr]
groupBySpace acc@(c:rest) curr = if curr == "" 
                                 then "":acc 
                                 else (first ++ curr):rest
                                 where first = if c == "" then c else c ++ " "

checkIfValid :: String -> Bool
checkIfValid xs = and $ map (`isInfixOf` xs) requiredFields

day4 :: String -> Int
day4 str = let allLines = lines str
               strippedSpaces = foldl groupBySpace [] allLines
           in  foldl (\x a -> if a then x + 1 else x) 0 $ map checkIfValid strippedSpaces

-- Part 2
data Field = BYR | IYR | EYR | HGT | HCL | ECL | PID 
stringFieldMap = [("byr", BYR), ("iyr", IYR), ("eyr", EYR), ("hgt", HGT), ("hcl", HCL), ("ecl", ECL), ("pid", PID)]
data Height = US | Metric

getHeight :: String -> (Int, Height)
getHeight x = let maybei = findIndex (\c -> c == 'i' || c == 'c') x
                  h = if "in" `isInfixOf` x then US else Metric
               in case maybei of 
                       (Just i) -> (read $ take i x, h) 
                       Nothing -> (0, US)

getPredicate :: Field -> (String -> Bool)
getPredicate BYR x = let val = read x :: Int in length x == 4 && val >= 1920 && val <= 2002
getPredicate IYR x = let val = read x :: Int in length x == 4 && val >= 2010 && val <= 2020
getPredicate EYR x = let val = read x :: Int in length x == 4 && val >= 2020 && val <= 2030
getPredicate HGT x = let (hgt, unit) = getHeight x
                      in case unit of
                              US -> hgt >= 59 && hgt <= 76
                              _  -> hgt >= 150 && hgt <= 193
getPredicate HCL (x:xs) = x == '#' && length xs == 6
getPredicate ECL x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
getPredicate PID x = length x == 9 && (and $ map (`elem` ['0'..'9']) x)

checkIfFieldValid :: String -> Bool
checkIfFieldValid str = let (strField, (_:val)) = splitAt 3 str
                            field = find (\(x, fld) -> x == strField) stringFieldMap
                         in case field of 
                                 Just (_, field) -> getPredicate field val
                                 Nothing -> True

-- day4Part2 :: String -> Int
day4Part2 str = let allLines = lines str
                    strippedSpaces = foldl groupBySpace [] allLines
                    hasFields = filter checkIfValid strippedSpaces
                    byWords = map words hasFields
                    bools   = map (\x -> and $ map checkIfFieldValid x) byWords
                in  foldl (\x a -> if a then x + 1 else x) 0 bools
