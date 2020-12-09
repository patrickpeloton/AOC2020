module Main where

import Data.List 
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7 
import Day8
import Day9

main :: IO ()
main = do
  contents <- readFile $ dayInput "day9"
  print $ day9Part2 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day