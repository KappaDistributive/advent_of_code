module Year2021.Day07 where

import qualified Data.List as L
import qualified Data.Text as T

fuelCost :: Int -> Int -> Int
fuelCost destination position = abs (destination - position)

accumulatedFuelCost :: Int -> Int -> Int
accumulatedFuelCost destination position = fst $ (n * (n + 1)) `divMod` 2
  where
    n = abs (destination - position)

parse :: String -> [Int]
parse x = map (read . T.unpack) $ T.splitOn (T.pack ",") (T.pack x)

partOne :: [Int] -> Int
partOne x =
  snd $
  L.minimumBy
    (\(_, a) (_, b) -> compare a b)
    [ (destination, sum $ map (fuelCost destination) x)
    | destination <- [(minimum x) .. (maximum x)]
    ]

partTwo :: [Int] -> Int
partTwo x =
  snd $
  L.minimumBy
    (\(_, a) (_, b) -> compare a b)
    [ (destination, sum $ map (accumulatedFuelCost destination) x)
    | destination <- [(minimum x) .. (maximum x)]
    ]

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
