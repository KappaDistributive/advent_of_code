module Year2015.Day04 where

import Crypto.Hash
import Data.ByteString.Char8 (pack)

numLeadingZeros :: String -> Int
numLeadingZeros ('0':xs) = 1 + numLeadingZeros xs
numLeadingZeros _ = 0

apply :: Int -> String -> String
apply x y = show $ hashWith MD5 (pack (y ++ show x))

step :: (Int, String) -> (Int, String)
step (x, y) = (x, apply x y)

partOne :: String -> Int
partOne x = fst . head $ filter (\a -> ((>= 5) . numLeadingZeros . snd) a) y
  where
    y = [step (b, x) | b <- [0 ..]]

partTwo :: String -> Int
partTwo x = fst . head $ filter (\a -> ((>= 6) . numLeadingZeros . snd) a) y
  where
    y = [step (b, x) | b <- [0 ..]]

run :: String -> IO ()
run contents = do
  let input = contents
  print $ partOne input
  print $ partTwo input
