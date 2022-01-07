module Year2017.Day06
  ( run
  ) where

import qualified Data.List as L
import qualified Data.Ord as O

parse :: String -> [(Int, Int)]
parse contents = zip (map (read :: String -> Int) $ words contents) [0 ..]

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

selectBlock' :: Int -> [(Int, Int)] -> [(Int, Int)]
select _ [] = []

selectBlock' max (x:xs) =
  if max == fst x
    then (x : xs)
    else selectBlock' max (rotate (x : xs))

selectBlock :: [(Int, Int)] -> [(Int, Int)]
selectBlock blocks =
  selectBlock'
    (fst (L.maximumBy (O.comparing fst) blocks))
    (L.sortBy (O.comparing snd) blocks)

distribute :: Int -> [(Int, Int)] -> [(Int, Int)]
distribute _ [] = []
distribute value blocks@(x:xs) =
  if value <= 0
    then blocks
    else distribute (value - 1) $ rotate (((fst x) + 1, snd x) : xs)

step :: [(Int, Int)] -> [(Int, Int)]
step [] = []
step blocks = distribute value (rotate ((0, index) : tail rotated_blocks))
  where
    rotated_blocks@(x:xs) = selectBlock blocks
    value = fst x
    index = snd x

partOne' :: Int -> [[(Int, Int)]] -> [(Int, Int)] -> (Int, [(Int, Int)])
partOne' result cache blocks =
  if sorted_blocks `elem` cache
    then (result, sorted_blocks)
    else partOne' (result + 1) (sorted_blocks : cache) (step blocks)
  where
    sorted_blocks = L.sortBy (O.comparing snd) blocks

partOne :: [(Int, Int)] -> Int
partOne blocks = fst $ partOne' 0 [] blocks

partTwo :: [(Int, Int)] -> Int
partTwo blocks = partOne (snd $ partOne' 0 [] blocks)

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
