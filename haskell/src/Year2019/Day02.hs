module Year2019.Day02
  ( run
  ) where

import qualified Data.Text as T

step :: (Int, [Int]) -> Maybe (Int, [Int])
step (index, opcodes) =
  if index < 0 || index >= length opcodes
    then Nothing
    else case opcodes !! index of
           1 ->
             Just
               ( index + 4
               , take addr_store opcodes ++
                 [param_0 + param_1] ++ drop (addr_store + 1) opcodes)
           2 ->
             Just
               ( index + 4
               , take addr_store opcodes ++
                 [param_0 * param_1] ++ drop (addr_store + 1) opcodes)
           99 -> Just (-1, opcodes)
           _ -> Nothing
  where
    param_0 = opcodes !! (opcodes !! (index + 1))
    param_1 = opcodes !! (opcodes !! (index + 2))
    addr_store = opcodes !! (index + 3)

execute' :: Maybe (Int, [Int]) -> Maybe (Int, [Int])
execute' (Just (index, opcodes)) =
  if index == -1
    then Just (index, opcodes)
    else execute' $ step (index, opcodes)
execute' Nothing = Nothing

execute :: [Int] -> Maybe [Int]
execute opcodes =
  case execute' (Just (0, opcodes)) of
    Just (index, result) -> Just result
    Nothing -> Nothing

simulate :: Int -> Int -> [Int] -> Int
simulate noun verb opcodes =
  case result of
    Just opcodes -> head opcodes
    Nothing -> -1
  where
    result = execute ([head opcodes] ++ [noun, verb] ++ drop 3 opcodes)

partOne :: [Int] -> Int
partOne = simulate 12 2

run contents = do
  let input =
        map ((read :: String -> Int) . T.unpack) $
        (T.splitOn (T.pack ",") . T.pack) contents
  print $ partOne input
