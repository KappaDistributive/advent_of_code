module Year2021.Day10 where

import qualified Data.Text as T

parse :: String -> [String]
parse x = map T.unpack $ T.splitOn (T.pack "\n") (T.pack x)

penalty :: Char -> Int
penalty x =
  case x of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> 0

partner :: Char -> Char
partner x =
  case x of
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    '<' -> '>'
    ')' -> '('
    ']' -> '['
    '}' -> '{'
    '>' -> '<'
    _ -> x

check' :: [Char] -> String -> (Maybe Char, [Char])
check' stack@(s:ss) (x:xs) =
  case x of
    '(' -> check' ('(' : stack) xs
    '[' -> check' ('[' : stack) xs
    '{' -> check' ('{' : stack) xs
    '<' -> check' ('<' : stack) xs
    ')' ->
      if partner s == x
        then check' ss xs
        else (Just x, stack)
    ']' ->
      if partner s == x
        then check' ss xs
        else (Just x, stack)
    '}' ->
      if partner s == x
        then check' ss xs
        else (Just x, stack)
    '>' ->
      if partner s == x
        then check' ss xs
        else (Just x, stack)
    _ -> check' stack xs
check' [] (x:xs) =
  case x of
    '(' -> check' ['('] xs
    '[' -> check' ['['] xs
    '{' -> check' ['{'] xs
    '<' -> check' ['<'] xs
    ')' -> (Just x, [])
    ']' -> (Just x, [])
    '}' -> (Just x, [])
    '>' -> (Just x, [])
    _ -> check' [] xs
check' stack [] = (Nothing, stack)

check :: String -> (Maybe Char, [Char])
check = check' []

partOne' :: Int -> [(Maybe Char, [Char])] -> Int
partOne' p ((Just c, _):ss) = partOne' (p + penalty c) ss
partOne' p ((Nothing, _):ss) = partOne' p ss
partOne' p [] = p

partOne = partOne' 0 . map check

run contents = do
  let input = parse contents
  print $ partOne input
