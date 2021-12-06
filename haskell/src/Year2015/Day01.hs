module Year2015.Day01 (run) where
import Data.Char

decode :: Char -> Int
decode x = case x of
  '(' -> 1
  ')' -> -1
  _ -> 0

-- partOne :: String -> Int
partOne = sum . map decode

-- partTwo  = 

readInt :: String -> Int
readInt = read

run contents = do
  print $ partOne contents
  -- print $ partTwo x
