module Year2022.Day01
  ( run
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import Text.Read (readMaybe)

parse :: String -> Maybe [[Int]]
parse x = mapM sequence calories
  where
    groups =
      filter (/= []) $
      map (lines . T.unpack) $ T.splitOn (T.pack "\n\n") $ T.pack x
    calories = [[readMaybe a | a <- group] | group <- groups]

partOne :: Maybe [[Int]] -> Int
partOne (Just groups) = head . reverse . L.sort $ map sum groups
partOne Nothing = -1

partTwo :: Maybe [[Int]] -> Int
partTwo (Just groups) = sum . (take 3) . reverse . L.sort $ map sum groups
partTwo Nothing = -1

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
