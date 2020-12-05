module Main where

import Data.List 
import Day1
import Day2
import Day3
import Day4
import Day5

main :: IO ()
main = do
  contents <- readFile $ dayInput "day5"
  print $ day5Part2 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day