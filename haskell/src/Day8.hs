module Day8
  (
    day8
  , day8Part2
  ) where

import Data.List
import Text.Regex.TDFA

-- Part 1
data Command = Nop | Acc | Jmp deriving (Show, Eq)

type Instruction = (Command, Int)

instance Read Command where
  readsPrec _ c
    | c == "nop" = [(Nop, "")]
    | c == "acc" = [(Acc, "")]
    | c == "jmp" = [(Jmp, "")]
    | otherwise = []

getValue :: String -> Int
getValue str = read (str =~ "-?[0-9]+")

cmdValTuple :: String -> Instruction
cmdValTuple str = let cmd = head $ words str
                      val = getValue str
                  in (read cmd, val)

getAddVal :: Command -> Int -> Int -> (Int, Int)
getAddVal Nop _ i = (0, i + 1)
getAddVal Acc val i = (val, i + 1)
getAddVal Jmp val i = (0, val + i)

runInstrcs :: (Int, [Int]) -> [Instruction] -> Int -> Int
runInstrcs (acc, indices) instrcs i = let (cmd, val) = instrcs !! i
                                          (addVal, nexti) = getAddVal cmd val i
                                          newAcc = addVal + acc
                                      in  if nexti `elem` indices
                                          then newAcc
                                          else runInstrcs (newAcc, nexti:indices) instrcs nexti

day8 :: String -> Int
day8 str = let allLines = lines str
               instrcs = map cmdValTuple allLines
            in runInstrcs (0, []) instrcs 0

-- Part2
swapInstrc :: Instruction -> Instruction
swapInstrc (Nop, val) = (Jmp, val)
swapInstrc (Jmp, val) = (Nop, val)
swapInstrc x = x

terminatesNormally :: (Int, [Int]) -> [Instruction] -> Int -> (Bool, Int)
terminatesNormally (acc, indices) instrcs i = let (cmd, val) = instrcs !! i
                                                  (addVal, nexti) = getAddVal cmd val i
                                                  newAcc = addVal + acc
                                              in  if nexti `elem` indices
                                                  then (False, newAcc)
                                                  else 
                                                    if nexti == length instrcs
                                                      then (True, newAcc)
                                                      else terminatesNormally (newAcc, nexti:indices) instrcs nexti

swapIndex :: Int -> [Instruction] -> [Instruction]
swapIndex i instrcs = let (l, (r:rs)) = splitAt i instrcs
                       in l ++ swapInstrc r : rs

runTerminator :: [[Instruction]] -> Int
runTerminator [] = 0
runTerminator (instrcs:rstInstcs) = let (b, val) = terminatesNormally (0, []) instrcs 0
                                    in  if b then val else runTerminator rstInstcs

-- day8Part2 :: String -> Int
day8Part2 str = let allLines = lines str
                    instrcs = map cmdValTuple allLines
                    indicesToSwap = findIndices (\(cmd, _) -> cmd == Jmp || cmd == Nop) instrcs
                    swappedInstrcs = foldl (\acc curr -> swapIndex curr instrcs:acc) [] indicesToSwap
                 in runTerminator swappedInstrcs
