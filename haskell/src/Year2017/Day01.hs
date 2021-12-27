module Year2017.Day01
  ( run
  ) where

import Data.Char (ord)

parse :: String -> [Int]
parse text = [ord character - ord '0' | character <- filter (/= '\n') text]

pairUp :: Bool -> [Int] -> [(Int, Int)]
pairUp False xs = zip xs (tail xs ++ [head xs])
pairUp True _ = []

partOne :: [Int] -> Int
partOne (x:xs) =
  if uncurry (==) p
    then x + partOne xs
    else partOne xs
  where
    (p:ps) = pairUp False (x : xs)
partOne _ = 0

-- partTwo :: [Int] -> Int
run contents = do
  let input = parse contents
  print $ partOne input
