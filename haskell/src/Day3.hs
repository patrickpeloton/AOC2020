module Day3 
  (
    day3
  , day3Part2
  ) where

-- Part 1
run = 3 -- first slope is right 3 down 1

isTree :: Int -> String -> Bool
isTree pos landscape = landscape !! pos == '#' 

countTrees :: Int -> (Int, Int) -> String -> (Int, Int)
countTrees run (n, t) landscape = let pos = (n * run) `mod` length landscape
                                  in  if isTree pos landscape then (n + 1, t + 1) else (n + 1, t)

day3 :: String -> Int
day3 str = let allLines = lines str
               (_, numTrees) = foldl (countTrees run) (0, 0) allLines
           in  numTrees

-- Part 2
filteredIndex :: (Int -> Bool) -> [a] -> [a]
filteredIndex p xs = [x | (x, i) <- zip xs [0..], p i]

countFilteredTrees :: [String] -> (Int, Int) -> Int
countFilteredTrees landscape (x, y) = let filteredTrees = filteredIndex ((== 0) . (`mod` y)) landscape
                                          (_, numTrees) = foldl (countTrees x) (0, 0) filteredTrees
                                      in  numTrees

day3Part2 :: String -> Int
day3Part2 str = let allLines = lines str
                    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
                    trees = map (countFilteredTrees allLines) slopes
                in  product trees