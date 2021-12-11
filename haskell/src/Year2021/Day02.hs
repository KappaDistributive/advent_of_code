module Year2021.Day02 where

import Data.Maybe (mapMaybe)

readInt :: String -> Int
readInt = read

parse' :: [String] -> Maybe (String, Int)
parse' [x, y] = Just (x, readInt y)
parse' _ = Nothing

parse :: String -> [(String, Int)]
parse x = mapMaybe (parse' . words) (lines x)

horizontal :: (String, Int) -> Int
horizontal (x, y) =
  if x == "forward"
    then y
    else 0

depth :: (String, Int) -> Int
depth (x, y) =
  case x of
    "down" -> y
    "up" -> -y
    _ -> 0

aim :: [(String, Int)] -> [Int]
aim x = scanl1 (+) (map depth x)

multiply :: (Int, Int) -> Int
multiply (x, y) = x * y

depthTwo :: [(String, Int)] -> [Int]
depthTwo x = zipWith (curry multiply) (map horizontal x) (aim x)

partOne :: [(String, Int)] -> Int
partOne x = (*) (sum $ map horizontal x) (sum $ map depth x)

partTwo :: [(String, Int)] -> Int
partTwo x = (*) (sum $ map horizontal x) ((sum . depthTwo) x)

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
