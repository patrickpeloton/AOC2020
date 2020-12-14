module Day11
  (
    day11
  ) where

-- Part1
data Seat = Occupied | Empty | Floor deriving (Show, Eq)

instance Read Seat where
  readsPrec _ c
    | c == "L" = [(Empty, "")]
    | c == "#" = [(Occupied, "")]
    | c == "." = [(Floor, "")]
    | otherwise = []

getSafeIndex :: [[a]] -> Int -> Int -> [a]
getSafeIndex as x y = [(as !! y) !! x | y < length as && y >= 0 && x < length (head as) && x >= 0]

isCenter :: Int -> Int -> Int -> Int -> Bool
isCenter c col r row = c == col && r == row

getNeighbors :: [[Seat]] -> Int -> Int -> [Seat]
getNeighbors seats row col = let rows = [row - 1 .. row + 1]
                                 cols = [col - 1 .. col + 1]
                              in concat [getSafeIndex seats c r | c <- cols, r <- rows, not $ isCenter c col r row]

numOccupiedAdjacent :: [Seat] -> Int
numOccupiedAdjacent = foldl (\acc curr -> if curr == Occupied then acc + 1 else acc) 0 

getNextState :: [Seat] -> Seat -> Seat
getNextState seats seat = case seat of
                               Empty -> if numOccupiedAdjacent seats == 0 then Occupied else Empty
                               Occupied -> if numOccupiedAdjacent seats >= 4 then Empty else Occupied
                               Floor -> Floor
                              
getNextBoard :: [[Seat]] -> [[Seat]]
getNextBoard seats = let xs = [0..length (head seats) - 1]
                         ys = [0..length seats - 1]
                      in map (\y -> map (\x -> getNextState (getNeighbors seats y x) ((seats !! y) !! x)) xs) ys

checkBoards :: [[Seat]] -> [[Seat]]
checkBoards seats = let nextSeats = getNextBoard seats
                     in if nextSeats == seats then seats else checkBoards nextSeats

countOccupiedSeats :: [[Seat]] -> Int
countOccupiedSeats = foldl (\acc curr -> acc + numOccupiedAdjacent curr) 0 

day11 :: String -> Int
day11 str = let allLines = map (map (\c -> read [c])) (lines str) :: [[Seat]]
             in countOccupiedSeats $ checkBoards allLines