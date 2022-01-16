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

partOne :: [String] -> Int
partOne map = length $ filter (== '#') path
  where
    path = trace map 3 1

run contents = do
  let input = parse contents
  print $ partOne input
