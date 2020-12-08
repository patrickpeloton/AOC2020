module Day1 
  (
    day1
  , day1Part2
  ) where

import Data.List 

-- Part1
findNums :: [Int] -> [Int] -> Int
findNums [] _ = 0
findNums (x:xs) ys = let numToFind = 2020 - x
                         maybeN = find (== numToFind) ys
                     in case maybeN of
                       (Just n) -> n * x
                       _ -> findNums xs ys

day1 :: String -> Int
day1 str = let nums = map read $ lines str
           in  findNums nums (tail nums)

-- Part 2
generateFirstList :: [Int] -> [(Int, Int)]
generateFirstList xs = concatMap (\y -> map (\x -> (x, y)) xs) xs

findNums2 :: [(Int, Int)] -> [Int] -> Int
findNums2 [] _ = 0
findNums2 ((x, y):xs) ys = let numToFind = 2020 - x - y
                               maybeN = find (== numToFind) ys
                            in case maybeN of
                                    (Just n) -> n * x * y
                                    _ -> findNums2 xs ys

  
day1Part2 :: String -> Int
day1Part2 str = let nums = map read $ lines str
                    firstTwoCombined = generateFirstList nums
                 in findNums2 firstTwoCombined nums
