module Year2017.Day05
  ( run
  ) where

import qualified Data.List as L

step :: (Int, [(Int, Int)]) -> Maybe (Int, [(Int, Int)])
step (index, instructions) =
  if index < 0 || index >= length instructions
    then Nothing
    else Just (new_index, new_instructions)
  where
    new_index =
      index + fst (instructions !! index) + snd (instructions !! index)
    new_instructions =
      ((L.take index instructions) ++
       [(fst (instructions !! index), snd (instructions !! index) + 1)]) ++
      (L.drop (index + 1) instructions)

stepTwo :: (Int, [(Int, Int)]) -> Maybe (Int, [(Int, Int)])
stepTwo (index, instructions) =
  if index < 0 || index >= length instructions
    then Nothing
    else Just (new_index, new_instructions)
  where
    new_index =
      index + fst (instructions !! index) + snd (instructions !! index)
    offset = fst (instructions !! index) + snd (instructions !! index)
    new_instructions =
      ((L.take index instructions) ++
       [ ( fst (instructions !! index)
         , if offset < 3
             then snd (instructions !! index) + 1
             else snd (instructions !! index) - 1)
       ]) ++
      (L.drop (index + 1) instructions)

partOne' :: Int -> Maybe (Int, [(Int, Int)]) -> Int
partOne' count state =
  case state of
    Just xs -> partOne' (count + 1) (step xs)
    Nothing -> count - 1

partOne :: [(Int, Int)] -> Int
partOne instructions = partOne' 0 (Just (0, instructions))

partTwo' :: Int -> Maybe (Int, [(Int, Int)]) -> Int
partTwo' count state =
  case state of
    Just xs -> partTwo' (count + 1) (stepTwo xs)
    Nothing -> count - 1

partTwo :: [(Int, Int)] -> Int
partTwo instructions = partTwo' 0 (Just (0, instructions))

run contents = do
  let input = zip (map (read :: String -> Int) $ lines contents) (L.cycle [0])
  print $ partOne input
  print $ partTwo input
