{-# LANGUAGE TupleSections #-}

module Year2021.Day05 where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Regex.TDFA

data Line =
  Line Int Int Int Int
  deriving (Eq, Ord, Show)

parseLine :: String -> Line
parseLine x = Line a b c d
  where
    matches = x =~ "[0-9]+" :: [[String]]
    [a, b, c, d] = map ((\c -> read c :: Int) . (!! 0)) matches

parse :: String -> [Line]
parse x =
  map parseLine $
  filter (/= "") $ map T.unpack $ T.splitOn (T.pack "\n") (T.pack x)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

unpack :: Bool -> Line -> [(Int, Int)]
unpack False (Line a b c d)
  | a == c = map (a, ) [(min b d) .. (max b d)] -- vertical line
  | b == d = map (, b) [(min a c) .. (max a c)] -- horizontal line
  | otherwise = []
unpack True (Line a b c d)
  | a == c = map (a, ) [(min b d) .. (max b d)] -- vertical line
  | b == d = map (, b) [(min a c) .. (max a c)] -- horizontal line
  | a < c && b < d = [(a + x, b + x) | x <- [0 .. (c - a)]] -- diagonal line `/`
  | a < c && b > d = [(a + x, b - x) | x <- [0 .. (c - a)]] -- diagonal line `\`
  | a > c && b < d = [(a - x, b + x) | x <- [0 .. (a - c)]] -- diagonal line `\`
  | a > c && b > d = [(a - x, b - x) | x <- [0 .. (a - c)]] -- diagonal line `/`
  | otherwise = []

partOne :: [Line] -> Int
partOne x =
  length $ filter ((> 1) . snd) $ frequency $ concatMap (unpack False) x

partTwo :: [Line] -> Int
partTwo x =
  length $ filter ((> 1) . snd) $ frequency $ concatMap (unpack True) x

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
