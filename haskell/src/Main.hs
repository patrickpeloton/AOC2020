module Main where

import Data.List 
import Day1
import Day2
import Day3

main :: IO ()
main = do
  contents <- readFile $ dayInput "day3"
  print $ day3Part2 contents

dayInput :: String -> String
dayInput day = "./inputs/" ++ day