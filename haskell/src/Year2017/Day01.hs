module Year2017.Day01
  ( run
  ) where

import Data.Char (ord)

parse :: String -> [Int]
parse text = [ord character - ord '0' | character <- filter (/='\n') text]

partOne :: [Int] -> Int
partOne (a:b:xs) = if a == b then a + partOne (b:xs) else partOne (b:xs)
partOne _ = 0

run contents = do
  let input = parse contents
  print $ partOne (input ++ [head input])
