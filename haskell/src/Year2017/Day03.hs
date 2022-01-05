module Year2017.Day03
  ( run
  ) where

data Direction
  = North
  | East
  | South
  | West
  deriving (Show)

-- Cell x y direction layer number
data Cell =
  Cell Int Int Direction Int Int
  deriving (Show)

startCell = Cell 0 0 East 0 1

parse :: String -> Int
parse = read

manhattenDistance :: Cell -> Cell -> Int
manhattenDistance (Cell a b _ _ _) (Cell c d _ _ _) = abs (a - c) + abs (b - d)

rotate :: Direction -> Direction
rotate direction =
  case direction of
    North -> West
    West -> South
    South -> East
    East -> North

move :: Direction -> Cell -> Cell
move direction (Cell x y d l n) =
  case direction of
    North -> Cell x (y + 1) d l n
    East -> Cell (x + 1) y d l n
    South -> Cell x (y - 1) d l n
    West -> Cell (x - 1) y d l n

shouldTurn :: Cell -> Bool
shouldTurn (Cell x y direction layer number) =
  case direction of
    North -> y == layer
    East -> x == layer + 1
    South -> y == -layer
    West -> x == -layer

endOfSpiral :: Cell -> Bool
endOfSpiral cell@(Cell _ _ direction _ _) =
  case direction of
    East -> shouldTurn cell
    _ -> False

-- 17< 16< 15< 14< 13
-- v               ^
-- 18   5 < 4 < 3  12
-- v    v       ^  ^
-- 19   6   1 > 2  11
-- v    v          ^
-- 20   7 > 8 > 9 >10
-- v
-- 21 >22 >23---> ...
step :: Cell -> Cell
step cell@(Cell x y direction layer number) =
  Cell new_x new_y new_direction new_layer (number + 1)
  where
    (Cell new_x new_y _ _ _) = move direction cell
    new_layer =
      if endOfSpiral (Cell new_x new_y direction layer number)
        then layer + 1
        else layer
    new_direction =
      if shouldTurn (Cell new_x new_y direction layer number)
        then rotate direction
        else direction

partOne' :: Int -> Cell -> Int
partOne' target cell@(Cell x y direction layer number) =
  if target == number
    then manhattenDistance startCell cell
    else partOne' target (step cell)

partOne :: Int -> Int
partOne target = partOne' target startCell

run contents = do
  let input = parse contents
  print $ partOne input
