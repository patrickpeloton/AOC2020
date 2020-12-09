module Day9
  (
    day9
  , day9Part2
  ) where

import Data.List

-- Part 1
preambleLength = 25

checkNums :: [Int] -> Int -> Bool
checkNums xs n = any ((`elem` xs) . (n - )) xs

runChecker :: [Int] -> Int -> Int
runChecker nums i = let (l, r:_) = splitAt i nums
                        numsToCheck = drop (length l - preambleLength) l
                     in if checkNums numsToCheck r
                        then runChecker nums (i + 1)
                        else r

day9 :: String -> Int
day9 str = let allLines = lines str
               nums = map read allLines :: [Int]
            in runChecker nums preambleLength

-- Part 2
getValidWindow :: [[Int]] -> Int -> Maybe [Int]
getValidWindow xs n = find (\x -> sum x == n) xs

-- findContiguousSet :: [Int] -> Int -> Int -> [Int]
findContiguousSet hay needle windowSize = let windows = map (\x -> take windowSize (drop x hay)) [0..(length hay - windowSize)]
                                              validWindow = getValidWindow windows needle
                                           in case validWindow of
                                                   (Just xs) -> xs
                                                   Nothing -> findContiguousSet hay needle (windowSize + 1)
day9Part2 :: String -> Int
day9Part2 str = let allLines = lines str
                    nums = map read allLines :: [Int]
                    invalidNum = runChecker nums preambleLength
                    (Just i) = invalidNum `elemIndex` nums
                    hay = take i nums
                    contigSet = findContiguousSet hay invalidNum 1
                 in maximum contigSet + minimum contigSet