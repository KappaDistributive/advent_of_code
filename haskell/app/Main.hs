module Main where

import System.Environment ( getArgs )

-- Year 2015
import Year2015.Day01 ( run )
import Year2015.Day02 ( run )
import Year2015.Day03 ( run )

-- Year 2021
import Year2021.Day01 ( run )
import Year2021.Day02 ( run )
import Year2021.Day03 ( run )
import Year2021.Day04 ( run )
import Year2021.Day05 ( run )


runDay 2015 01 = Year2015.Day01.run
runDay 2015 02 = Year2015.Day02.run
runDay 2015 03 = Year2015.Day03.run

runDay 2021 01 = Year2021.Day01.run
runDay 2021 02 = Year2021.Day02.run
runDay 2021 03 = Year2021.Day03.run
runDay 2021 04 = Year2021.Day04.run
runDay 2021 05 = Year2021.Day05.run


path :: [String] -> String
path x
  | length x > 2 && x !! 2 == "mock" = "../" ++ year ++ "/data/input_" ++ day ++ "_mock.txt"
  | otherwise = "../" ++ year ++ "/data/input_" ++ day ++ ".txt"
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
