module Year2021.Day03 where

import Data.Array

parse :: String -> [[Bool]]
parse x = map decode (lines x)

decode :: String -> [Bool]
decode = map (== '1')

decodeBinary' :: [Int] -> Int
decodeBinary' [] = 0
decodeBinary' (x:xs) = x + 2 * decodeBinary' xs

decodeBinary :: [Int] -> Int
decodeBinary = decodeBinary' . reverse

position :: (Int, [[a]]) -> [a]
position (pos, x:xs) = (x !! pos) : position (pos, xs)
position (_, _) = []

countOnes :: Int -> [[Bool]] -> Int
countOnes pos codes = sum $ map fromEnum (position (pos, codes))

countAllOnes :: [[Bool]] -> [Int]
countAllOnes x = map (`countOnes` x) [0,1 .. length (head x) - 1]

gamma :: [[Bool]] -> Int
gamma x =
  decodeBinary $
  map
    (\n ->
       if 2 * n > length x
         then 1
         else 0)
    (countAllOnes x)

epislon :: [[Bool]] -> Int
epislon x =
  decodeBinary $
  map
    (\n ->
       if 2 * n < length x
         then 1
         else 0)
    (countAllOnes x)

digitsCounts :: Int -> [[Bool]] -> (Int, Int)
digitsCounts n x = (length x - countOnes n x, countOnes n x)

mostCommonDigit n x d =
  if uncurry (==) counts
    then d
    else uncurry (<) counts
  where
    counts = digitsCounts n x

leastCommonDigit n x d =
  if uncurry (==) counts
    then d
    else uncurry (>) counts
  where
    counts = digitsCounts n x

oxygenGeneratorRating' :: Int -> [[Bool]] -> (Int, [[Bool]], Bool)
oxygenGeneratorRating' pos x = do
  (pos + 1, filter (\d -> (d !! pos) == digit) x, digit)
  where
    digit = mostCommonDigit pos x True

oxygenGeneratorRating :: Int -> [[Bool]] -> [Bool]
oxygenGeneratorRating pos x =
  if pos == length (head x)
    then []
    else digit : oxygenGeneratorRating next_pos output
  where
    (next_pos, output, digit) = oxygenGeneratorRating' pos x

scrubberRating' :: Int -> [[Bool]] -> (Int, [[Bool]], Bool)
scrubberRating' pos x = do
  (pos + 1, filter (\d -> (d !! pos) == digit) x, digit)
  where
    digit = leastCommonDigit pos x False

scrubberRating :: Int -> [[Bool]] -> [Bool]
scrubberRating pos x
  | pos == length (head x) = []
  | length x == 1 = head x !! pos : scrubberRating next_pos x
  | otherwise = digit : scrubberRating next_pos output
  where
    (next_pos, output, digit) = scrubberRating' pos x

partOne :: [[Bool]] -> Int
partOne x = gamma x * epislon x

partTwo :: [[Bool]] -> Int
partTwo x =
  (*)
    (decodeBinary $ map fromEnum (oxygenGeneratorRating 0 x))
    (decodeBinary $ map fromEnum (scrubberRating 0 x))

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
