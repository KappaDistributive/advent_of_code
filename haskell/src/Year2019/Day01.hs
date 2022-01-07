module Year2019.Day01
  ( run
  ) where

fuel :: Bool -> Int -> Int
fuel False mass = max 0 ((mass `div` 3) - 2)
fuel True 0 = 0
fuel True mass = value + fuel True value
  where
    value = max 0 ((mass `div` 3) - 2)

partOne :: [Int] -> Int
partOne modules = sum $ map (fuel False) modules

partTwo :: [Int] -> Int
partTwo modules = sum $ map (fuel True) modules

run contents = do
  let input = map (read :: String -> Int) $ lines contents
  print $ partOne input
  print $ partTwo input
