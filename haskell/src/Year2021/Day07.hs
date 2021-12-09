module Year2021.Day07 where

import qualified Data.List as L
import qualified Data.Text as T

fuelCost :: Int -> Int -> Int
fuelCost destination position = abs (destination - position)

parse :: String -> [Int]
parse x = map ((read) . T.unpack) $ T.splitOn (T.pack ",") (T.pack x)

partOne :: [Int] -> Int
partOne x =
  snd . head $
  L.sortBy
    (\(_, a) (_, b) -> compare a b)
    [ (destination, sum $ map (fuelCost destination) x)
    | destination <- [(minimum x) .. (maximum x)]
    ]

run contents = do
  let input = parse contents
  print $ partOne input
