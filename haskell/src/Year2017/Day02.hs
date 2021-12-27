module Year2017.Day02
  ( run
  ) where

import Data.List (tails)

parse :: String -> [[Int]]
parse text = [[read entry | entry <- line] | line <- map words $ lines text]

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

sieve :: [(Int, Int)] -> Int
sieve ((x, y):ps)
  | x `mod` y == 0 = x `div` y 
  | y `mod` x == 0 = y `div` x
  | otherwise = sieve ps
sieve _ = 0

partOne :: [[Int]] -> Int
partOne spreadsheet = sum [maximum row - minimum row | row <- spreadsheet]

partTwo :: [[Int]] -> Int
partTwo spreadsheet = sum [sieve $ pairs row | row <- spreadsheet]

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
