module Year2017.Day02
  ( run
  ) where

parse :: String -> [[Int]]
parse text = [[read entry | entry <- line] | line <- map words $ lines text]

partOne :: [[Int]] -> Int
partOne spreadsheet = sum [maximum row - minimum row | row <- spreadsheet]

run contents = do
  let input = parse contents
  print $ partOne input
