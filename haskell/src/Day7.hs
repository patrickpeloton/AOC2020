module Day7
  (
    day7
  , day7Part2
  ) where

import Data.List
import Text.Regex.TDFA

-- Part 1
type BagTree = (String, [String])

getBagColors :: String -> BagTree
getBagColors str = let (_, parent, children) = str =~ "[a-z]+ [a-z]+" :: (String, String, String)
                       matchedChildren = getAllTextMatches (children =~ "[1-9] [a-z]+ [a-z]+") :: [String]
                    in (parent, matchedChildren)

matchBag :: String -> String -> Bool
matchBag color1 color2 = color2 =~ ("[1-9] " ++ color1)

containsBag :: String -> BagTree -> Bool
containsBag color (_, colors) = any (matchBag color) colors

goldBag = "shiny gold"

getParents :: [BagTree] -> [String]
getParents = map fst

getBags :: String -> [BagTree] -> [String]
getBags color bagColors = nub $ getParents $ filter (containsBag color) bagColors

goThroughParents :: [BagTree] -> [String] -> [String]
goThroughParents _ [] = []
goThroughParents allBags (p:ps) = let parents = getBags p allBags
                                   in (p:goThroughParents allBags (nub (ps ++ parents)))

day7 :: String -> Int
day7 str = let allLines = lines str
               allBagColors = map getBagColors allLines
            in length (nub $ goThroughParents allBagColors [goldBag]) - 1

-- Part 2
getBagNumber :: String -> Int
getBagNumber s = read (s =~ "[0-9]+" :: String)

getColor :: String -> String
getColor str = str =~ "[a-z]+ [a-z]+"

getAllBagChildren :: String -> [BagTree] -> BagTree
getAllBagChildren colorAndNum bagColors = let color = getColor colorAndNum
                                              (Just bag) = find (\(c, _) -> c == color) bagColors
                                           in bag

sumBags :: Int -> String -> String
sumBags n str = let bagNum = getBagNumber str              
                    color = getColor str
                in  show (bagNum * n) ++ " " ++ color
-- goThroughChildren :: [BagTree] -> [String] -> Int
-- 1 dotted violet -> 4 striped white
-- 1 striped white -> 3 light white
-- 1 light white -> 2 vibrant brown
-- 1 vibrant brown -> none
goThroughChildren allBags p = let (_, allChildren) = getAllBagChildren p allBags
                                  bagNum = getBagNumber p
                               in if not (null allChildren)
                                    then bagNum + (sum $ map (\c -> goThroughChildren allBags (sumBags bagNum c)) allChildren)
                                    else bagNum

day7Part2 str = let allLines = lines str
                    allBagColors = map getBagColors allLines
                    (_, allGoldBagChildren) = getAllBagChildren goldBag allBagColors
                 in sum $ map (goThroughChildren allBagColors) allGoldBagChildren