module Year2021.Day06 where

import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Read (decimal)

frequency :: (Ord a) => [a] -> M.Map a Int
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

parse :: String -> M.Map Int Int
parse x = frequency timers
  where
    timers =
      map fst $ E.rights $ map decimal $ T.splitOn (T.pack ",") (T.pack x)

transform :: [(Int, Int)] -> [(Int, Int)]
transform [] = []
transform [x@(timer, count)]
  | timer == 0 = [(6, count), (8, count)]
  | otherwise = [(timer - 1, count)]
transform (x@(timer, count):xs)
  | timer == 0 = (6, count) : (8, count) : transform xs
  | otherwise = (timer - 1, count) : transform xs

step :: Int -> M.Map Int Int -> M.Map Int Int
step 0 m = m
step k m = step (k - 1) $ M.fromListWith (+) $ (transform . M.toList) m

partOne m = sum $ map snd $ M.toList (step 80 m)

run contents = do
  let input = parse contents
  print $ partOne input
