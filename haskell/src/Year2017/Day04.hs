module Year2017.Day04
  ( run
  ) where

import qualified Data.List as L

hasDuplicate :: (Eq a) => [a] -> Bool
hasDuplicate [] = False
hasDuplicate (x:xs) = x `elem` xs || hasDuplicate xs

hasWeakDuplicate :: (Eq a, Ord a) => [[a]] -> Bool
hasWeakDuplicate xs = hasDuplicate $ map L.sort xs

partOne :: [[String]] -> Int
partOne input =
  sum
    [ if hasDuplicate x
      then 0
      else 1
    | x <- input
    ]

partTwo :: [[String]] -> Int
partTwo input =
  sum
    [ if hasWeakDuplicate x
      then 0
      else 1
    | x <- input
    ]

run contents = do
  let input = map words $ lines contents
  print $ partOne input
  print $ partTwo input
