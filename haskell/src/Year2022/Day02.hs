module Year2022.Day02 ( run ) where
import Text.Read (readMaybe)

reveal :: Char -> Char
reveal 'A' = 'R'
reveal 'X' = 'R'
reveal 'B' = 'P'
reveal 'Y' = 'P'
reveal 'C' = 'S'
reveal 'Z' = 'S'
reveal _ = '_'

parse :: String -> [(Char, Char)]
parse [] = []
parse x = map (\y -> (reveal . head $ y, reveal . last $ y )) xs
  where
    xs = lines x


value :: Char -> Int
value 'R' = 1
value 'P' = 2
value 'S' = 3
value _ = 0


score :: (Char, Char) -> Int
score ('R', 'R') = 3
score ('R', 'P') = 6
score ('R', 'S') = 0
score ('P', 'R') = 0
score ('P', 'P') = 3
score ('P', 'S') = 6
score ('S', 'R') = 6
score ('S', 'P') = 0
score ('S', 'S') = 3
score _ = -1

partOne :: [(Char, Char)] -> Int
partOne moves = sum $ map (\(a, b) -> a + b) $ zip (map score moves) (map (value . snd) moves)

partTwo _ = 2

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
