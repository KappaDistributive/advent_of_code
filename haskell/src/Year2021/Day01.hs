module Year2021.Day01
  ( run
  ) where

import Data.Char

buildPairs [] = []
buildPairs [x] = []
buildPairs (x:y:ys) = (x, y) : buildPairs (y : ys)

buildTriples [] = []
buildTriples [x] = []
buildTriples [x, y] = []
buildTriples (x:y:z:zs) = (x, y, z) : buildTriples (y : z : zs)

isIncreased :: [(Int, Int)] -> [Int]
isIncreased =
  map
    (\x ->
       if uncurry (<) x
         then 1
         else 0)

sumTriples (x, y, z) = x + y + z

input = map readInt . words

partOne = sum . isIncreased . buildPairs

partTwo x = partOne $ map sumTriples (buildTriples x)

readInt :: String -> Int
readInt = read

run contents = do
  let x = input contents
  print $ partOne x
  print $ partTwo x
