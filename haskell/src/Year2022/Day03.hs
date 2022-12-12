module Year2022.Day03
  ( run
  ) where

import Data.Char (ord)
import Data.List (intersect)

priority :: Char -> Int
priority item
  | 'a' <= item && item <= 'z' = ord item - ord 'a' + 1
  | 'A' <= item && item <= 'Z' = ord item - ord 'A' + 27
  | otherwise = 0

halve :: [a] -> ([a], [a])
halve xs = splitAt s xs
  where
    s = (length xs) `div` 2

partOne :: String -> Int
partOne x = sum $ map (priority . head . uncurry intersect) y
  where
    y = map halve $ lines x

partTwo _ = 2

run contents = do
  let input = contents
  print $ partOne input
  print $ partTwo input
