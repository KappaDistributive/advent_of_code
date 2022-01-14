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

chains' :: String -> String -> [String]
chains' (x:xs) buffer =
  if null buffer || x == head buffer
    then chains' xs (x : buffer)
    else buffer : chains' xs [x]
chains' [] buffer = [buffer]

chains :: String -> [String]
chains word = chains' word []

chainLengths :: String -> [Int]
chainLengths word = map length $ chains word

hasPair :: Bool -> Int -> Bool
hasPair False number = any (uncurry (==)) pairs
  where
    digits = show number
    pairs = zip digits (tail digits)
hasPair True number = 2 `elem` chainLengths digits
  where
    digits = show number

partOne :: (Int, Int) -> Int
partOne (lower, upper) =
  length $
  filter
    (== True)
    [hasPair False number && isIncreasing number | number <- [lower .. upper]]

partTwo :: (Int, Int) -> Int
partTwo (lower, upper) =
  length $
  filter
    (== True)
    [hasPair True number && isIncreasing number | number <- [lower .. upper]]

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
