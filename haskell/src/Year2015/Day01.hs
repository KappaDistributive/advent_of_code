module Year2015.Day01
  ( run
  ) where

import Data.Char

decode :: Char -> Int
decode x =
  case x of
    '(' -> 1
    ')' -> -1
    _ -> 0

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0,1 .. length xs - 1] xs

partOne :: String -> Int
partOne = sum . map decode

partTwo :: String -> Int
partTwo x =
  fst . head $
  filter (\x -> snd x == -1) (enumerate (scanl (+) 0 (map decode x)))

readInt :: String -> Int
readInt = read

run contents = do
  print $ partOne contents
  print $ partTwo contents
