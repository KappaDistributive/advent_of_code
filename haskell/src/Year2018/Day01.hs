module Year2018.Day01
  ( run
  ) where

import Data.Text (pack, replace, unpack)

parse :: String -> [Int]
parse contents =
  map (read . unpack . replace (pack "+") (pack "") . pack) $ lines contents

partOne' :: [Int] -> [Int] -> Int -> Int
partOne' [] _ frequency = frequency
partOne' changes frequencies frequency =
  partOne' (tail changes) (frequency : frequencies) (frequency + head changes)

partOne :: [Int] -> Int
partOne input = partOne' input [] 0

run contents = do
  let input = parse contents
  print $ partOne input
