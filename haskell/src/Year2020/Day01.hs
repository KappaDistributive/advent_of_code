module Year2020.Day01
  ( run
  ) where

import qualified Data.List as L

parse :: String -> [Int]
parse contents = map (read :: String -> Int) $ lines contents

pairs :: [Int] -> [(Int, Int)]
pairs numbers = [(x, y) | (x:ys) <- L.tails numbers, y <- ys]

partOne :: [(Int, Int)] -> Maybe Int
partOne (x:xs) =
  if a + b == 2020
    then Just (a * b)
    else partOne xs
  where
    (a, b) = x
partOne [] = Nothing

run contents = do
  let input = parse contents
  print $ partOne $ pairs input
