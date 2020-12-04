module Day2 
  (
    day2
  , day2Part2
  ) where

import Data.List.Split

-- ["19-20","h:","hhhhhhhhhhhvhhhhhhhr"] -> (19, 20, 'h', "hhhhhhhhhhhvhhhhhhhr")
formatLines :: [String] -> (Int, Int, Char, String)
formatLines (count:char:pass:_) = let (low:high:_) = splitOn "-" count
                                  in  (read low, read high, head char, pass)

-- Part 1
isValidPassword :: (Int, Int, Char, String) -> Bool
isValidPassword (l, h, c, pass) = let filteredPass = filter (== c) pass
                                      numCs = length filteredPass
                                  in  numCs >= l && numCs <= h

day2 :: String -> Int
day2 str = let allLines = map words $ lines str
               formattedLines = map formatLines allLines
               validPasswords = filter isValidPassword formattedLines
           in  length validPasswords

-- Part 2
xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

isValidPasswordPart2 :: (Int, Int, Char, String) -> Bool
isValidPasswordPart2 (l, h, c, pass) = let pos1 = pass !! (l - 1)
                                           pos2 = pass !! (h - 1)
                                       in  xor (pos1 == c) (pos2 == c)

day2Part2 :: String -> Int
day2Part2 str = let allLines = map words $ lines str
                    formattedLines = map formatLines allLines
                    validPasswords = filter isValidPasswordPart2 formattedLines
                in  length validPasswords