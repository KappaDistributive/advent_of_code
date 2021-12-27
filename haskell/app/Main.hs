module Main where

import System.Environment (getArgs)

-- Year 2015
import Year2015.Day01 (run)
import Year2015.Day02 (run)
import Year2015.Day03 (run)
import Year2015.Day04 (run)
import Year2015.Day05 (run)
import Year2015.Day06 (run)

-- Year 2016
import Year2016.Day01 (run)
import Year2016.Day03 (run)
import Year2016.Day04 (run)

-- Year 2017
import Year2017.Day01 (run)

-- Year 2021
import Year2021.Day01 (run)
import Year2021.Day02 (run)
import Year2021.Day03 (run)
import Year2021.Day04 (run)
import Year2021.Day05 (run)
import Year2021.Day06 (run)
import Year2021.Day07 (run)

import Year2021.Day10 (run)

-- Year 2015
runDay 2015 01 = Year2015.Day01.run
runDay 2015 02 = Year2015.Day02.run
runDay 2015 03 = Year2015.Day03.run
runDay 2015 04 = Year2015.Day04.run
runDay 2015 05 = Year2015.Day05.run
runDay 2015 06 = Year2015.Day06.run

-- Year 2016
runDay 2016 01 = Year2016.Day01.run
runDay 2016 03 = Year2016.Day03.run
runDay 2016 04 = Year2016.Day04.run

-- Year 2017
runDay 2017 01 = Year2017.Day01.run

-- Year 2021
runDay 2021 01 = Year2021.Day01.run
runDay 2021 02 = Year2021.Day02.run
runDay 2021 03 = Year2021.Day03.run
runDay 2021 04 = Year2021.Day04.run
runDay 2021 05 = Year2021.Day05.run
runDay 2021 06 = Year2021.Day06.run
runDay 2021 07 = Year2021.Day07.run
runDay 2021 10 = Year2021.Day10.run

path :: [String] -> String
path x
  | length x > 2 && x !! 2 == "mock" =
    "../data/" ++ year ++ "/input_" ++ day ++ "_mock.txt"
  | otherwise = "../data/" ++ year ++ "/input_" ++ day ++ ".txt"
  where
    year = head x
    day = x !! 1

-- main :: IO ()
main = do
  args <- getArgs
  let year = head args
  let day = args !! 1
  contents <- readFile (path args)
  runDay (read year) (read day) contents
