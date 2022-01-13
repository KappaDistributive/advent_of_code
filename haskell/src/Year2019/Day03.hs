module Year2019.Day03
  ( run
  ) where

import qualified Data.List as L
import qualified Data.Text as T

data Move
  = MoveUp Int
  | MoveDown Int
  | MoveLeft Int
  | MoveRight Int
  deriving (Show)

data Point =
  Point Int Int
  deriving (Eq, Show)

pack :: String -> Maybe Move
pack [] = Nothing
pack (x:xs) =
  case x of
    'U' -> Just (MoveUp (read xs))
    'D' -> Just (MoveDown (read xs))
    'L' -> Just (MoveLeft (read xs))
    'R' -> Just (MoveRight (read xs))
    _ -> Nothing

parse :: String -> Maybe ([Move], [Move])
parse contents =
  case lhs_moves of
    Nothing -> Nothing
    Just l ->
      case rhs_moves of
        Nothing -> Nothing
        Just r -> Just (l, r)
  where
    l =
      [ map T.unpack $ T.splitOn (T.pack ",") (T.pack line)
      | line <- lines contents
      ]
    lhs = head l
    rhs = l !! 1
    lhs_moves = mapM pack lhs
    rhs_moves = mapM pack rhs

move' :: [Move] -> [Point] -> [Point]
move' [] p = p
move' m [] = move' m [Point 0 0]
move' (m:ms) (p:ps) = move' ms (move'' m p ++ (p : ps))

move'' :: Move -> Point -> [Point]
move'' m (Point x y) =
  case m of
    MoveUp d -> reverse [Point x (y + e) | e <- [1 .. d]]
    MoveDown d -> reverse [Point x (y - e) | e <- [1 .. d]]
    MoveLeft d -> reverse [Point (x - e) y | e <- [1 .. d]]
    MoveRight d -> reverse [Point (x + e) y | e <- [1 .. d]]

move :: [Move] -> [Point]
move m = move' m []

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point x y) (Point x' y') = abs (x - x') + abs (y - y')

partOne' :: [Point] -> [Point] -> Int
partOne' l r =
  minimum $
  map (manhattenDistance (Point 0 0)) $
  filter (/= Point 0 0) (l `L.intersect` r)

partOne :: Maybe ([Move], [Move]) -> Maybe Int
partOne input =
  case input of
    Nothing -> Nothing
    Just (lhs, rhs) -> Just $ partOne' (move lhs) (move rhs)

run contents = do
  let input = parse contents
  print $ partOne input
