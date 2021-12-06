module Main where

import System.Environment ( getArgs )
import Year2015.Day01 ( run )

import Year2021.Day01 ( run )
import Year2021.Day02 ( run )
import Year2021.Day03 ( run )


runDay 2015 01 = Year2015.Day01.run

runDay 2021 01 = Year2021.Day01.run
runDay 2021 02 = Year2021.Day02.run
runDay 2021 03 = Year2021.Day03.run

-- main :: IO ()
main = do 
  args <- getArgs
  let year = head args
  let day = args !! 1
  contents <- readFile $ "../" ++ year ++ "/data/input_" ++ day ++ ".txt"
  runDay (read year) (read day) contents
