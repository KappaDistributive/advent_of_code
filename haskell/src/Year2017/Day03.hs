module Year2017.Day03
  ( run
  ) where

data Direction
  = North
  | East
  | South
  | West
  deriving (Show)

data Position =
  Position Int Int
  deriving (Show)

parse :: String -> Int
parse = read

manhattenDistance :: Position -> Position -> Int
manhattenDistance (Position a b) (Position c d) = abs (a - c) + abs (b - d)

rotate :: Direction -> Direction
rotate direction =
  case direction of
    North -> West
    West -> South
    South -> East
    East -> North

move :: Direction -> Position -> Position
move direction (Position x y) =
  case direction of
    North -> Position x (y + 1)
    East -> Position (x + 1) y
    South -> Position x (y - 1)
    West -> Position (x - 1) y

shouldTurn :: (Position, Direction, Int) -> Bool
shouldTurn (Position x y, direction, spiral) =
  case direction of
    North -> y == spiral
    East -> x == spiral + 1
    South -> y == -spiral
    West -> x == -spiral

endOfSpiral :: (Position, Direction, Int) -> Bool
endOfSpiral (position, direction, spiral) =
  case direction of
    East -> shouldTurn (position, direction, spiral)
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
step :: (Position, Direction, Int, Int) -> (Position, Direction, Int, Int)
step (Position 0 0, _, _, _) = (Position 1 0, North, 1, 2)
step (position, direction, spiral, number) =
  (new_position, new_direction, new_spiral, number + 1)
  where
    new_position = move direction position
    new_spiral =
      if endOfSpiral (new_position, direction, spiral)
        then spiral + 1
        else spiral
    new_direction =
      if shouldTurn (new_position, direction, spiral)
        then rotate direction
        else direction

partOne' :: Int -> (Position, Direction, Int, Int) -> Int
partOne' target (position, direction, spiral, number) =
  if target == number
    then manhattenDistance (Position 0 0) position
    else partOne' target (step (position, direction, spiral, number))

partOne :: Int -> Int
partOne target = partOne' target (Position 0 0, North, 0, 1)

run contents = do
  let input = parse contents
  print $ partOne input
