module Year2015.Day06 where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data Op a = OpOn a | OpOff a | OpToggle a
  deriving (Show)

bound :: Int
bound = 999

boardEnglish :: M.Map (Int, Int) Bool
boardEnglish = M.fromList [((x,y),False) | x <-[0..bound], y <- [0..bound]]

boardElvish :: M.Map (Int, Int) Int
boardElvish = M.fromList [((x,y),0) | x <-[0..bound], y <- [0..bound]]

runOpEnglish :: Op (Int, Int, Int, Int) -> M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
runOpEnglish op = M.mapWithKey (executeEnglish op)

runOpElvish :: Op (Int, Int, Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
runOpElvish op = M.mapWithKey (executeElvish op)

runOpsEnglish :: [Op (Int, Int, Int, Int)] -> M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
runOpsEnglish [] = id
runOpsEnglish [op] = runOpEnglish op
runOpsEnglish (op:ops) = runOpsEnglish ops . runOpEnglish op

runOpsElvish :: [Op (Int, Int, Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
runOpsElvish [] = id
runOpsElvish [op] = runOpElvish op
runOpsElvish (op:ops) = runOpsElvish ops . runOpElvish op

executeEnglish :: Op (Int, Int, Int, Int) -> (Int, Int) -> (Bool -> Bool)
executeEnglish (OpOff (min_x, min_y, max_x, max_y)) (x,y) = if min_x <= x && x <= max_x && min_y <= y && y <= max_y then const False else id
executeEnglish (OpOn (min_x, min_y, max_x, max_y)) (x,y) = if min_x <= x && x <= max_x && min_y <= y && y <= max_y then const True else id
executeEnglish (OpToggle (min_x, min_y, max_x, max_y)) (x,y) = if min_x <= x && x <= max_x && min_y <= y && y <= max_y then not else id

executeElvish :: Op (Int, Int, Int, Int) -> (Int, Int) -> (Int -> Int)
executeElvish (OpOff (min_x, min_y, max_x, max_y)) (x,y) v = if min_x <= x && x <= max_x && min_y <= y && y <= max_y then 0 `max` (v-1) else v
executeElvish (OpOn (min_x, min_y, max_x, max_y)) (x,y) v = if min_x <= x && x <= max_x && min_y <= y && y <= max_y then v+1 else v
executeElvish (OpToggle (min_x, min_y, max_x, max_y)) (x,y) v = if min_x <= x && x <= max_x && min_y <= y && y <= max_y then v+ 2 else v

parseLine :: String -> Maybe (Op (Int, Int, Int, Int))
parseLine x = case op_group of
  "turn on" -> Just $ OpOn (read min_x, read min_y, read max_x, read max_y)
  "turn off" -> Just $ OpOff (read min_x, read min_y, read max_x, read max_y)
  "toggle" -> Just $ OpToggle (read min_x, read min_y, read max_x, read max_y)
  _ -> Nothing
  where (_,_,_,[op_group, min_x, min_y, max_x, max_y]) =  (x =~ "(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)") :: (String,String,String,[String])

parse :: String -> Maybe [Op (Int, Int, Int, Int)]
parse x  = sequence $ parseLine <$> lines
  where lines = map T.unpack $ T.splitOn (T.pack "\n") (T.pack x)

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

partOne :: Maybe [Op (Int, Int, Int, Int)] -> Maybe Int
partOne (Just ops) = Just (length . M.filter (==True) $ runOpsEnglish ops boardEnglish)
partOne Nothing = Nothing

partTwo :: Maybe [Op (Int, Int, Int, Int)] -> Maybe Int
partTwo (Just ops) = Just (M.foldl (+) 0 $ runOpsElvish ops boardElvish)
partTwo Nothing = Nothing


run :: String -> IO ()
run contents = do
  let input = parse contents
  -- print $ input
  -- print $ parseLine "turn on 0,0 through 999,999"
  -- print $ parseLine "toggle 0,0 through 999,0"
  -- print $ parseLine "turn off 499,499 through 500,500"
  -- print $ runOp (OpToggle (0,0,999,0)) board
  print $ partOne input
  print $ partTwo input
