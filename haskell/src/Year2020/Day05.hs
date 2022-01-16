module Year2020.Day05 where

data Move
  = F
  | B
  | L
  | R
  deriving (Show)

row' :: [Move] -> Int -> Int -> Int
row' (m:ms) lower upper =
  case m of
    F -> row' ms lower (lower + ((upper - lower) `div` 2))
    B -> row' ms (lower + ((upper - lower) `div` 2)) upper
    _ -> -1
row' [] lower upper = upper

row :: [Move] -> Int
row moves = row' (take 7 moves) 0 127

column' :: [Move] -> Int -> Int -> Int
column' (m:ms) lower upper =
  case m of
    L -> column' ms lower (lower + ((upper - lower) `div` 2))
    R -> column' ms (lower + ((upper - lower) `div` 2)) upper
    _ -> -1
column' [] lower upper = upper

column :: [Move] -> Int
column moves = column' (reverse $ take 3 $ reverse moves) 0 7

seat :: [Move] -> Int
seat moves = 8 * row moves + column moves

decode :: Char -> Move
decode x
  | x == 'F' = F
  | x == 'B' = B
  | x == 'L' = L
  | x == 'R' = R
  | otherwise = R

parse' :: String -> [Move]
parse' = map decode

parse :: String -> [[Move]]
parse contents = map parse' $ lines contents

partOne :: [[Move]] -> Int
partOne moves = maximum $ map seat moves

run contents = do
  let input = parse contents
  print $ partOne input
