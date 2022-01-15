module Year2020.Day01
  ( run
  ) where

import qualified Data.List as L

parse :: String -> [Int]
parse contents = map (read :: String -> Int) $ lines contents

pairs :: [Int] -> [(Int, Int)]
pairs numbers = [(x, y) | (x:ys) <- L.tails numbers, y <- ys]

triples :: [Int] -> [(Int, Int, Int)]
triples numbers =
  [(x, y, z) | (x:ys) <- L.tails numbers, (y:zs) <- L.tails ys, z <- zs]

partOne :: [(Int, Int)] -> Maybe Int
partOne (x:xs) =
  if a + b == 2020
    then Just (a * b)
    else partOne xs
  where
    (a, b) = x
partOne [] = Nothing

partTwo :: [(Int, Int, Int)] -> Maybe Int
partTwo (x:xs) =
  if a + b + c == 2020
    then Just (a * b * c)
    else partTwo xs
  where
    (a, b, c) = x
partTwo [] = Nothing

run contents = do
  let input = parse contents
  print $ partOne $ pairs input
  print $ partTwo $ triples input
