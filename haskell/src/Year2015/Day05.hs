module Year2015.Day05 where

import qualified Data.Text as T

parse :: String -> [String]
parse = map T.unpack . T.splitOn (T.pack "\n") . T.pack

isVowel :: Char -> Bool
isVowel x =
  case x of
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    _ -> False

numVowels :: String -> Int
numVowels = length . filter isVowel

hasTwin :: String -> Bool
hasTwin (x:y:xs) = x == y || hasTwin (y : xs)
hasTwin _ = False

hasIllegalPattern :: String -> Bool
hasIllegalPattern (x:y:xs) =
  case (x, y) of
    ('a', 'b') -> True
    ('c', 'd') -> True
    ('p', 'q') -> True
    ('x', 'y') -> True
    _ -> hasIllegalPattern (y : xs)
hasIllegalPattern _ = False

partOne :: [String] -> Int
partOne =
  length .
  filter hasTwin .
  filter ((>= 3) . numVowels) . filter (not . hasIllegalPattern)

run :: String -> IO ()
run contents = do
  let input = parse contents
  print $ partOne input
