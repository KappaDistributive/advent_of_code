module Year2018.Day03 where

import qualified Data.List as L
import qualified Data.Map as M
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data Claim =
  Claim Int Int Int Int Int
  deriving (Show)

-- example claim: #1 @ 1,3: 4x4
parse' :: String -> Maybe Claim
parse' claim
  | length matches' == 1 =
    Just
      (Claim
         (read $ matches !! 1)
         (read $ matches !! 2)
         (read $ matches !! 3)
         (read $ matches !! 4)
         (read $ matches !! 5))
  | otherwise = Nothing
  where
    claim_regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
    matches' = (claim =~ claim_regex) :: [[String]]
    matches =
      if not (null matches')
        then head matches'
        else []

parse :: String -> Maybe [Claim]
parse contents = mapM parse' $ lines contents

claim' :: Int -> M.Map (Int, Int) [Int] -> (Int, Int) -> M.Map (Int, Int) [Int]
claim' identifier m k = M.insertWith (++) k [identifier] m

claim :: M.Map (Int, Int) [Int] -> Claim -> M.Map (Int, Int) [Int]
claim m c@(Claim identifier x y w h) =
  foldl
    (claim' identifier)
    m
    [(a, b) | a <- [x .. (x + w - 1)], b <- [y .. (y + h - 1)]]

partOne :: Maybe [Claim] -> Maybe Int
partOne claims =
  case claims of
    Just c ->
      Just $
      length $
      filter (\((x, y), l) -> length l > 1) $ M.toList $ foldl claim M.empty c
    Nothing -> Nothing

partTwo :: Maybe [Claim] -> Maybe [Int]
partTwo claims =
  case claims of
    Just c -> Just [x | x <- unique_claim_ids, x `notElem` multiple_claim_ids]
      where all_claims = M.toList $ foldl claim M.empty c
            multiple_claim_ids =
              L.nub $
              concatMap snd $ filter (\(_, l) -> length l > 1) all_claims
            unique_claim_ids =
              L.nub $
              concatMap snd $ filter (\(_, l) -> length l == 1) all_claims
    Nothing -> Nothing

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
