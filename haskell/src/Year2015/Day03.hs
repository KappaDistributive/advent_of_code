module Year2015.Day03 (run) where

import qualified Data.List as L

data Position = Position Int Int
  deriving (Eq, Show)

parse' :: String -> [(Char, Position)]
parse' [] = [('.', Position 0 0)]
parse' (x:xs) = case x of
  '^' -> ('^', Position a (b-1)) : parse' xs
  '>' -> ('>', Position (a+1) b) : parse' xs
  'v' -> ('v', Position a (b+1)) : parse' xs
  '<' -> ('<', Position (a-1) b) : parse' xs
  _ -> parse' xs
  where Position a b = snd . head $ parse' xs

parse :: String -> [(Char, Position)]
parse = reverse . parse' . reverse

partOne :: String -> Int
partOne = length . L.nub . map snd . parse

-- partTwo :: String -> Int
-- partTwo = print ""

run contents = do
  print $ partOne contents
  -- print $ partTwo contents
