module Year2018.Day04 where

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
  = BeginShift String Int
  | FallAsleep String

timeRegex = "\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\]"

parse contents =
  (instructions =~ timeRegex) :: (String, String, String, [String])
  where
    instructions = head $ lines contents

run contents = do
  let input = parse contents
  print input
  print (TimeStamp 2022 1 23 11 23)
