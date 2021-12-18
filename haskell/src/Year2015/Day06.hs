module Year2015.Day06 where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data Op a
  = OpOn a
  | OpOff a
  | OpToggle a
  deriving (Show)

bound :: Int
bound = 999

coordinates :: [(Int, Int)]
coordinates = [(x, y) | x <- [0 .. bound], y <- [0 .. bound]]

executeEnglish :: Op (Int, Int, Int, Int) -> (Int, Int) -> (Bool -> Bool)
executeEnglish (OpOff (min_x, min_y, max_x, max_y)) (x, y) =
  if min_x <= x && x <= max_x && min_y <= y && y <= max_y
    then const False
    else id
executeEnglish (OpOn (min_x, min_y, max_x, max_y)) (x, y) =
  if min_x <= x && x <= max_x && min_y <= y && y <= max_y
    then const True
    else id
executeEnglish (OpToggle (min_x, min_y, max_x, max_y)) (x, y) =
  if min_x <= x && x <= max_x && min_y <= y && y <= max_y
    then not
    else id

executeElvish :: Op (Int, Int, Int, Int) -> (Int, Int) -> (Int -> Int)
executeElvish (OpOff (min_x, min_y, max_x, max_y)) (x, y) v =
  if min_x <= x && x <= max_x && min_y <= y && y <= max_y
    then 0 `max` (v - 1)
    else v
executeElvish (OpOn (min_x, min_y, max_x, max_y)) (x, y) v =
  if min_x <= x && x <= max_x && min_y <= y && y <= max_y
    then v + 1
    else v
executeElvish (OpToggle (min_x, min_y, max_x, max_y)) (x, y) v =
  if min_x <= x && x <= max_x && min_y <= y && y <= max_y
    then v + 2
    else v

parseLine :: String -> Maybe (Op (Int, Int, Int, Int))
parseLine x =
  case op_group of
    "turn on" -> Just $ OpOn (read min_x, read min_y, read max_x, read max_y)
    "turn off" -> Just $ OpOff (read min_x, read min_y, read max_x, read max_y)
    "toggle" -> Just $ OpToggle (read min_x, read min_y, read max_x, read max_y)
    _ -> Nothing
  where
    (_, _, _, [op_group, min_x, min_y, max_x, max_y]) =
      (x =~
       "(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)") :: ( String
                                                                                   , String
                                                                                   , String
                                                                                   , [String])

parse :: String -> Maybe [Op (Int, Int, Int, Int)]
parse x = sequence $ parseLine <$> lines
  where
    lines = map T.unpack $ T.splitOn (T.pack "\n") (T.pack x)

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

partOne :: Maybe [Op (Int, Int, Int, Int)] -> Maybe Int
partOne (Just ops) =
  Just . length $
  filter (== True) $
  map
    (\(x, y) -> compose [executeEnglish op (x, y) | op <- ops] False)
    coordinates
partOne Nothing = Nothing

partTwo :: Maybe [Op (Int, Int, Int, Int)] -> Maybe Int
partTwo (Just ops) =
  Just . sum $
  map (\(x, y) -> compose [executeElvish op (x, y) | op <- ops] 0) coordinates
partTwo Nothing = Nothing

run :: String -> IO ()
run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
