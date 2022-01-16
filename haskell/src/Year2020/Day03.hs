module Year2020.Day03
  ( run
  ) where

parse :: String -> [String]
parse = lines

trace' :: [String] -> Int -> Int -> Int -> Int -> String
trace' map vel_x vel_y x y =
  if y >= length map
    then ""
    else (line !! (x `mod` length line)) :
         trace' map vel_x vel_y (x + vel_x) (y + vel_y)
  where
    line = map !! y

trace :: [String] -> Int -> Int -> String
trace map vel_x vel_y = trace' map vel_x vel_y 0 0

numTrees :: [String] -> Int -> Int -> Int
numTrees m vel_x vel_y = length $ filter (== '#') $ trace m vel_x vel_y

partOne :: [String] -> Int
partOne m = numTrees m 3 1

partTwo :: [String] -> Int
partTwo m = product [uncurry (numTrees m) x | x <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
