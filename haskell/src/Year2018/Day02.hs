module Year2018.Day02
  ( run
  ) where

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

run contents = do
  let input = lines contents
  print $ partOne input
