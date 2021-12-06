module Year2021.Day04 where
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Either
import Data.Text.Read (decimal)

-- Board numbers state winning_number
data Board = Board [Int] [Bool] Int
  deriving (Eq, Show)

parseMoves :: String -> [Int]
parseMoves = map fst . Data.Either.rights . map decimal . T.splitOn (T.pack ",") . T.pack . head . lines

parseBoard :: T.Text -> Board
parseBoard x = Board numbers state (-1)
  where
    numbers = map fst . Data.Either.rights . map decimal . T.split (\ c -> c==' ' || c == '\n') $ x
    state = replicate 25 False

parseBoards :: String -> [Board]
parseBoards = map parseBoard . tail . T.splitOn (T.pack "\n\n") . T.pack

parse :: String -> ([Int], [Board])
parse x = (parseMoves x, parseBoards x)

updateState :: Maybe Int -> [Bool] -> [Bool]
updateState (Just index) state = fst splits ++ True : tail (snd splits)
  where
  splits = splitAt index state
updateState Nothing state = state

score :: Board -> Int
score (Board numbers state (-1)) = -1
score (Board numbers state winning_number) = winning_number * sum (map fst (filter (not . snd) (zip numbers state)))

winningMove :: [Int] -> Board -> Int
winningMove _ (Board _ _ (-1)) = -1
winningMove moves (Board _ _ winning_number) = case index of
  Just i -> i
  Nothing -> -1
  where
    index = L.elemIndex winning_number moves

slices :: [Int] -> [a] -> [a]
slices indices xs =map (xs !!) indices

completeRow :: Int -> Board -> Bool
completeRow row (Board _ state _ ) = all (==True) (slices indices state)
  where
    indices = map (+5*row) [0,1,2,3,4]

completeColumn:: Int -> Board -> Bool
completeColumn column (Board _ state _ ) = all (==True) (slices indices state)
  where
    indices = map (+column) [0,5,10,15,20]

hasWon :: Board -> Bool
hasWon board = any ((==True) . (`completeRow` board)) [0,1,2,3,4] || any ((==True) . (`completeColumn` board)) [0,1,2,3,4]

applyMove :: Int -> [Board] -> [Board]
applyMove number (x:xs) = Board numbers new_state new_winning_number : applyMove number xs
  where
    Board numbers state winning_number = x
    index = L.elemIndex number numbers
    new_state = if winning_number == -1 then updateState index state else state
    new_winning_number = if winning_number == -1 && hasWon (Board numbers new_state winning_number) then number else winning_number
applyMove number [] = []

step [] boards = [boards]
step moves boards = new_boards : step (tail moves) new_boards
  where new_boards = applyMove (head moves) boards


partOne :: ([Int], [Board]) -> Int
partOne x = score . head $ filter hasWon $ head $ filter (any hasWon) results
  where
    (moves, boards) = x
    results = step moves boards

partTwo:: ([Int], [Board]) -> Int
partTwo x = fst $ last sorted_winners
  where
    (moves, boards) = x
    results = step moves boards
    winners = filter hasWon $ last results
    sorted_winners = L.sortBy (\ (_,a) (_,b) -> compare a b) $ zip (map score winners) (map (winningMove moves) winners)


run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
