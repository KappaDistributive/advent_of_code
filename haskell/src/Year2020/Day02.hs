module Year2020.Day02
  ( run
  ) where

import qualified Data.Text as T

data Policy =
  Policy Char Int Int
  deriving (Show)

parse' :: String -> (Policy, String)
parse' content =
  ( Policy (head (splits' !! 1)) (read $ head limits) (read $ limits !! 1)
  , splits !! 1)
  where
    splits = map T.unpack $ T.splitOn (T.pack ": ") (T.pack content)
    splits' = map T.unpack $ T.splitOn (T.pack " ") (T.pack $ head splits)
    limits = map T.unpack $ T.splitOn (T.pack "-") (T.pack $ head splits')

parse :: String -> [(Policy, String)]
parse contents = map parse' $ lines contents

check :: Policy -> String -> Bool
check (Policy symbol lower upper) password = lower <= l && l <= upper
  where
    l = length $ filter (== symbol) password

partOne :: [(Policy, String)] -> Int
partOne [] = 0
partOne (x@(policy, password):xs) = score + partOne xs
  where
    score =
      if check policy password
        then 1
        else 0

run contents = do
  let input = parse contents
  print $ partOne input
