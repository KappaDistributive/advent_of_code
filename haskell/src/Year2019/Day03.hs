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
  | MoveEnd
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

trace' :: [Move] -> [(Point, Move)] -> [(Point, Move)]
trace' [] p = p
trace' _ [] = []
trace' ms (s@(Point x y, m):xs) = trace' (tail ms) (step : s : xs)
  where
    step =
      case m of
        MoveUp d -> (Point x (y + d), head ms)
        MoveDown d -> (Point x (y - d), head ms)
        MoveLeft d -> (Point (x - d) y, head ms)
        MoveRight d -> (Point (x + d) y, head ms)
        MoveEnd -> (Point x y, MoveEnd)

trace :: [Move] -> [(Point, Move)]
trace [] = []
trace m = reverse $ trace' (tail m ++ [MoveEnd]) [(Point 0 0, head m)]

unroll :: (Point, Move) -> [Point]
unroll (Point x y, m) =
  case m of
    MoveUp d -> [Point x (y + s) | s <- [1 .. d]]
    MoveDown d -> [Point x (y - s) | s <- [1 .. d]]
    MoveLeft d -> [Point (x - s) y | s <- [1 .. d]]
    MoveRight d -> [Point (x + s) y | s <- [1 .. d]]
    MoveEnd -> []

intersection :: (Point, Move) -> (Point, Move) -> [Point]
intersection (p, m) (p', m') =
  L.nub $ unroll (p, m) `L.intersect` unroll (p', m')

intersections :: [(Point, Move)] -> [(Point, Move)] -> [Point]
intersections x y = L.nub $ concat [intersection l r | l <- x, r <- y]

manhattenDistance :: Point -> Point -> Int
manhattenDistance (Point x y) (Point x' y') = abs (x - x') + abs (y - y')

partOne :: Maybe ([Move], [Move]) -> Maybe Int
partOne input =
  case input of
    Just (lhs, rhs) ->
      Just
        (minimum $
         map
           (manhattenDistance (Point 0 0))
           (intersections (trace lhs) (trace rhs)))
    Nothing -> Nothing

run contents = do
  let input = parse contents
  print $ partOne input
