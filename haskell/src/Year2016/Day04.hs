module Year2016.Day04 where

import Data.Map (fromListWith, toList)
import Data.Char (chr, ord)
import Data.List (sortBy)
import Data.Ord (Ordering(..))
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

data Room =
  Room String Int String
  deriving (Show)

roomRegex = "^([a-z\\-]+)\\-([0-9]+)\\[([a-z]+)\\]$"

compareFunction :: (Char, Int) -> (Char, Int) -> Ordering
compareFunction (a,b) (c, d) 
  | (a == c) && (b == d) = EQ
  | b > d = LT
  | (b == d) && (a < c) = LT
  | otherwise = GT

checksum :: Room -> String
checksum (Room name _ _ ) = take 5 $ map fst frequencies
  where
  letters = filter (\x -> x `elem` ['a'..'z']) name
  frequencies = sortBy compareFunction $ frequency letters

char2int :: Char -> Int
char2int x = ord x - ord 'a'

int2char :: Int -> Char
int2char x = chr (ord 'a' + x)

shift :: Int -> Char -> Char
shift _ '-' = '-'
shift offset x = int2char $ (char2int x + offset) `mod` 26

decrypt :: Int -> String -> String
decrypt offset = map (shift offset)

parseRoom :: String -> Maybe Room
parseRoom code =
  if (code =~ roomRegex) :: Bool
    then Just (Room name (read sectorId) checksum)
    else Nothing
  where
    (_, _, _, matches) =
      (code =~ roomRegex) :: (String, String, String, [String])
    [name, sectorId, checksum] = matches

parse :: [String] -> Maybe [Room]
parse = mapM parseRoom

partOne :: Maybe [Room] -> Maybe Int
partOne (Just rooms) = Just $ sum $ map (\(_,_,a) -> a) $ filter (\(a,b,_) -> a == b) $ map (\room@(Room _ sector_id c) -> (c, checksum room, sector_id)) rooms
partOne Nothing = Nothing

partTwo :: Maybe [Room] -> Maybe Int
partTwo (Just rooms) = Just $ snd . head $ filter (\ (a, b) -> a == "northpole-object-storage") $ map (\(Room name sectorId _) -> (decrypt sectorId name, sectorId)) $ rooms
partTwo Nothing = Nothing

run :: String -> IO ()
run contents = do
  let input = parse . lines $ contents
  print $ partOne input
  print $ partTwo input
