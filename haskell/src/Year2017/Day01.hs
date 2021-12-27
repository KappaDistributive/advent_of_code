module Year2017.Day01
  ( run
  ) where

import Data.Char (ord)

parse :: String -> [Int]
parse text = [ord character - ord '0' | character <- filter (/= '\n') text]

pairUp :: Bool -> [Int] -> [(Int, Int)]
pairUp part_two xs = zip xs (drop offset xss)
  where
    xss = cycle xs
    offset =
      if part_two
        then length xs `div` 2
        else 1

sumUp :: [(Int, Int)] -> Int
sumUp (x:xs) =
  if uncurry (==) x
    then fst x + sumUp xs
    else sumUp xs
sumUp [] = 0

run contents = do
  let input = parse contents
  print $ sumUp $ pairUp False input
  print $ sumUp $ pairUp True input
