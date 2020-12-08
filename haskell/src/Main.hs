module Main where

import Data.List 
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7 

main :: IO ()
main = do
  contents <- readFile $ dayInput "day7"
  print $ day7Part2 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day