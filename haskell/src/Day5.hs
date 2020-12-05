module Day5 
  (
    day5
  , day5Part2
  ) where

import Data.List

-- Part 1
data Seat = F | B | L | R deriving (Show, Eq)

instance Read Seat where
  readsPrec _ c
    | c == "F" = [(F, "")]
    | c == "B" = [(B, "")]
    | c == "L" = [(L, "")]
    | c == "R" = [(R, "")]
    | otherwise = []

binarySearch :: Int -> [Seat] -> Int
binarySearch _ [] = 0
binarySearch x (s:remSeats) = let halfway = x `div` 2
                              in case s of
                                      F -> binarySearch halfway remSeats
                                      L -> binarySearch halfway remSeats
                                      _ -> halfway + binarySearch halfway remSeats

separateRowsCols :: [Seat] -> ([Seat], [Seat])
separateRowsCols = span (\x -> x == F || x == B)

getSeatId :: [Seat] -> Int
getSeatId seats = let (fb, lr) = separateRowsCols seats
                      rowNum = binarySearch 128 fb
                      colNum = binarySearch 8 lr
                  in  rowNum * 8 + colNum

day5 :: String -> Int
day5 str = let allLines = lines str
               seats = [map (\x -> read [x]) l | l <- allLines] :: [[Seat]]
            in maximum $ map getSeatId seats

-- Part 2
splitByValue :: [Int] -> [[Int]]
splitByValue = foldl (\(xs:rem) x -> if null xs || x == 1 + head xs then (x:xs):rem else [x]:(xs:rem)) [[]]

findMissing :: [[Int]] -> Int
findMissing (_:x2) = head (head x2) + 1

day5Part2 str = let allLines = lines str
                    seats = [map (\x -> read [x]) l | l <- allLines] :: [[Seat]]
                 in findMissing $ splitByValue $ sort $ map getSeatId seats