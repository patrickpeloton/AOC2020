module Main where

import Data.List 
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

main :: IO ()
main = do
  contents <- readFile $ dayInput "day6"
  print $ day6Part2 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day