module Day10
  (
    day10
  , day10Part2
  ) where

import Data.List

-- Part 1
day10 :: String -> Int
day10 str = let parsedLines = sort $ map read $ lines str
                allLines = (0:parsedLines)
                pairs = zip (init allLines) (tail allLines) 
                (ones:threes:_) = group $ sort $ map (\(x, y) -> y - x) pairs
             in length ones * (length threes + 1)

-- Part 2
findCombinations :: Int -> [Int] -> [[Int]]
findCombinations n [] = [[n]]
findCombinations n ns = let (l, r) = partition (\x -> x <= n + 3 && x >= n - 3) ns
                         in concatMap (\x -> map (n:) (findCombinations x (delete x l ++ r))) l

-- day10Part2 :: String -> [Int]
day10Part2 str = let allLines = map read $ lines str ::[Int]
                  in findCombinations 0 [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]