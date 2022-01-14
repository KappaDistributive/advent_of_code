module Year2019.Day04
  ( run
  ) where

import qualified Data.List as L
import qualified Data.Text as T

parse :: String -> (Int, Int)
parse contents =
  if length splits == 2
    then (read $ head splits, read $ splits !! 1)
    else (-1, -1)
  where
    splits = map T.unpack $ T.splitOn (T.pack "-") (T.pack contents)

isIncreasing :: Int -> Bool
isIncreasing number = L.sort digits == digits
  where
    digits = show number

hasPair :: Int -> Bool
hasPair number = any (uncurry (==)) pairs
  where
    digits = show number
    pairs = zip digits (tail digits)

partOne :: (Int, Int) -> Int
partOne (lower, upper) =
  length $
  filter
    (== True)
    [hasPair number && isIncreasing number | number <- [lower .. upper]]

run contents = do
  let input = parse contents
  print $ partOne input
