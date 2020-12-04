module Day1 
  (
    day1
  ) where

import Data.List 


findNums :: [Int] -> [Int] -> Int
findNums [] _ = 0
findNums (x:xs) ys = let numToFind = 2020 - x
                         maybeN = find (== numToFind) ys
                     in case maybeN of
                       (Just n) -> n * x
                       _ -> findNums xs ys

findNumsPart2 :: [Int] -> [Int] -> Int
findNumsPart2 [] as = let (xs, ys) = splitAt (length as `div` 2) as in findNumsPart2 xs ys
findNumsPart2 as [] = let (xs, ys) = splitAt (length as `div` 2) as in findNumsPart2 xs ys
findNumsPart2 (x:xs) (y:ys) 
  | x + y > 2020 = findNumsPart2 xs ys
  | (x + y + last xs) > 2020 = case find (\z -> z + x + y == 2020) xs of
                               (Just n) -> n * x * y
                               _ -> findNumsPart2 (x:xs) ys
  | (x + y + last xs) < 2020 = case find (\z -> z + x + y == 2020) ys of
                                (Just n) -> n * x * y
                                _ -> findNumsPart2 xs (y:ys)

day1Part2 :: String -> Int
day1Part2 str = let nums = map read $ lines str
                    sortedNums = sort nums
                    half = (length nums `div` 2) + 1
                    (l, r) = splitAt half sortedNums
                 in findNumsPart2 l (reverse r)

day1 :: String -> Int
day1 str = let nums = map read $ lines str
           in  findNums nums (tail nums)