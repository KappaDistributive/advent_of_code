module Year2016.Day01 where

import qualified Data.Text as T

data Turn
  = TurnStay
  | TurnLeft
  | TurnRight
  deriving (Show)

data Orientation
  = OrientationNorth
  | OrientationEast
  | OrientationSouth
  | OrientationWest
  deriving (Show)

turn :: Turn -> Orientation -> Orientation
turn TurnStay orientation = orientation
turn TurnLeft orientation =
  case orientation of
    OrientationNorth -> OrientationWest
    OrientationEast -> OrientationNorth
    OrientationSouth -> OrientationEast
    OrientationWest -> OrientationSouth
turn TurnRight orientation =
  case orientation of
    OrientationNorth -> OrientationEast
    OrientationEast -> OrientationSouth
    OrientationSouth -> OrientationWest
    OrientationWest -> OrientationNorth

step :: Orientation -> Int -> (Int, Int) -> (Int, Int)
step OrientationNorth distance (x, y) = (x, y - distance)
step OrientationEast distance (x, y) = (x + distance, y)
step OrientationSouth distance (x, y) = (x, y + distance)
step OrientationWest distance (x, y) = (x - distance, y)

parseInstruction :: String -> Maybe (Turn, Int)
parseInstruction ('L':xs) = Just (TurnLeft, read xs)
parseInstruction ('R':xs) = Just (TurnRight, read xs)
parseInstruction _ = Nothing

manhattenDistance :: (Int, Int) -> (Int, Int) -> Int
manhattenDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

parse :: String -> Maybe [(Turn, Int)]
parse =
  mapM (parseInstruction . T.unpack) <$>
  T.splitOn (T.pack ", ") . T.pack . filter (/= '\n')

execute ::
     [(Turn, Int)] -> [(Orientation, (Int, Int))] -> [(Orientation, (Int, Int))]
execute ((turnOp, distance):instructions) (state@(orientation, position):path) =
  execute instructions (new_state : state : path)
  where
    new_orientation = turn turnOp orientation
    new_state = (new_orientation, step new_orientation distance position)
execute [] path = path
execute _ [] = []

tracePath :: [(Turn, Int)] -> [(Orientation, (Int, Int))]
tracePath instructions = execute instructions [(OrientationNorth, (0, 0))]

partOne :: Maybe [(Turn, Int)] -> Maybe Int
partOne (Just instructions) =
  Just . manhattenDistance (0, 0) . snd . head $ tracePath instructions
partOne Nothing = Nothing

firstDuplicate :: (Eq a) => [a] -> Maybe a
firstDuplicate (x:xs) =
  if x `elem` xs
    then Just x
    else firstDuplicate xs
firstDuplicate [] = Nothing

unroll_ :: (Turn, Int) -> [(Turn, Int)]
unroll_ (turnOp, distance) =
  if distance > 1
    then (turnOp, 1) : replicate (distance - 1) (TurnStay, 1)
    else [(turnOp, distance)]

unroll :: [(Turn, Int)] -> [(Turn, Int)]
unroll = concatMap unroll_

partTwo :: Maybe [(Turn, Int)] -> Maybe Int
partTwo (Just instructions) =
  case position of
    Just p -> Just $ manhattenDistance (0, 0) p
    Nothing -> Nothing
  where
    positions = map snd $ tracePath $ unroll instructions
    position = firstDuplicate $ reverse positions
partTwo Nothing = Nothing

run :: String -> IO ()
run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
