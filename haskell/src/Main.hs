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
import Day10
import Day11

main :: IO ()
main = do
  contents <- readFile $ dayInput "day11test"
  print $ day11 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day