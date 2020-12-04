module Main where

import Data.List 
import Day1
import Day2
import Day3
import Day4

main :: IO ()
main = do
  contents <- readFile $ dayInput "day4"
  print $ day4Part2 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day