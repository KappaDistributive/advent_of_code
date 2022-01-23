module Year2018.Day04 where

import Data.List (isInfixOf, sort)
import Text.Printf (printf)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data TimeStamp =
  TimeStamp Int Int Int Int Int
  deriving (Eq, Ord)

instance Show TimeStamp where
  show (TimeStamp year month day hour minute) =
    "[" ++
    printf "%04d" year ++
    "-" ++
    printf "%02d" month ++
    "-" ++
    printf "%02d" day ++
    " " ++ printf "%02d" hour ++ ":" ++ printf "%02d" minute ++ "]"

data Instruction
  = BeginShift TimeStamp Int
  | FallAsleep TimeStamp
  | WakeUp TimeStamp
  deriving (Eq, Show)

timeStamp :: Instruction -> TimeStamp
timeStamp (BeginShift ts _) = ts
timeStamp (FallAsleep ts) = ts
timeStamp (WakeUp ts) = ts

instance Ord Instruction where
  (<=) lhs rhs = timeStamp lhs <= timeStamp rhs

timeRegex = "\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\]"

extractTimeStamp :: String -> TimeStamp
extractTimeStamp instruction = TimeStamp (read $ head matches) (read $ matches !! 1) (read $ matches !! 2) (read $ matches !! 3) (read $ matches !! 4)
  where (_, _, _, matches) = (instruction =~ timeRegex) :: (String, String, String, [String])

opRegex = "#([0-9]+)"

extractOp :: String -> Int
extractOp instruction
  | "falls asleep" `isInfixOf` instruction = -2
  | "wakes up" `isInfixOf` instruction = -1
  | otherwise = read $ head matches
    where (_, _, _, matches) = (instruction =~ opRegex) :: (String, String, String, [String])

parse' :: String -> Instruction
parse' instruction = case op of
  -2 -> FallAsleep timestamp
  -1 -> WakeUp timestamp
  _ -> BeginShift timestamp op
  where timestamp = extractTimeStamp instruction
        op = extractOp instruction

parse contents = sort $ map parse' instructions
  where
    instructions = lines contents

run contents = do
  let input = parse contents
  print input
