module Year2018.Day02
  ( run
  ) where

import qualified Data.List as L
import qualified Data.Map as M

check :: Int -> String -> Bool
check target word =
  target `elem` (M.elems $ M.fromListWith (+) (zip word (repeat 1)))

partOne' :: Int -> Int -> [String] -> Int
partOne' target result [] = result
partOne' target result (word:words) =
  if check target word
    then partOne' target (result + 1) words
    else partOne' target result words

partOne :: [String] -> Int
partOne words = (partOne' 2 0 words) * (partOne' 3 0 words)

common :: (String, String) -> String
common ([], []) = ""
common ((x:xs), (y:ys)) =
  if x == y
    then x : common (xs, ys)
    else common (xs, ys)

pairs :: (Eq a) => [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- L.tails l, y <- ys]

partTwo :: [String] -> String
partTwo input = head $ filter ((== l) . length) $ map common $ pairs input
  where
    l = (length $ head input) - 1

run contents = do
  let input = lines contents
  print $ partOne input
  print $ partTwo input
