module Year2018.Day01
  ( run
  ) where

import Data.Text (pack, replace, unpack)

parse :: String -> [Int]
parse contents =
  map (read . unpack . replace (pack "+") (pack "") . pack) $ lines contents

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

partOne' :: [Int] -> [Int] -> Int -> Int
partOne' [] _ frequency = frequency
partOne' changes frequencies frequency =
  partOne' (tail changes) (frequency : frequencies) (frequency + head changes)

partTwo' :: [Int] -> [Int] -> Int -> Int
partTwo' [] _ frequency = frequency
partTwo' changes frequencies frequency =
  if frequency `elem` frequencies
    then frequency
    else partTwo'
           (rotate changes)
           (frequency : frequencies)
           (frequency + head changes)

partOne :: [Int] -> Int
partOne input = partOne' input [] 0

partTwo :: [Int] -> Int
partTwo input = partTwo' input [] 0

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
