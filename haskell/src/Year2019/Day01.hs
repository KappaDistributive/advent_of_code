module Year2019.Day01
  ( run
  ) where

partOne :: [Int] -> Int
partOne modules = sum $ map (\x -> max 0 ((x `div` 3) - 2)) modules

run contents = do
  let input = map (read :: String -> Int) $ lines contents
  print $ partOne input
