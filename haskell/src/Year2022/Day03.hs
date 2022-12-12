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

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

partOne :: String -> Int
partOne x = sum $ map (priority . head . uncurry intersect) y
  where
    y = map halve $ lines x

partTwo x = sum $ map (priority . head) z
  where
    y = group 3 $ lines x
    z = map (\a -> (head a `intersect` (a !! 1)) `intersect` (a !! 2)) y

run contents = do
  let input = contents
  print $ partOne input
  print $ partTwo input
