module Year2020.Day04 where

import qualified Data.Text as T

data Passport =
  Passport
    (Maybe Int) -- byr: birth year
    (Maybe Int) -- iyr: issue year
    (Maybe Int) -- eyr: expiration year
    (Maybe String) -- hgt: height
    (Maybe String) -- hcl: hair color
    (Maybe String) -- ecl: eye color
    (Maybe String) -- pid: passport id
    (Maybe String) -- cid: country id
  deriving (Eq, Show)

extract :: String -> String -> Maybe String
extract pattern passport =
  if length splits == 2
    then Just (T.unpack . head $ T.splitOn (T.pack " ") (T.pack (splits !! 1)))
    else Nothing
  where
    splits =
      map T.unpack $ T.splitOn (T.pack (pattern ++ ":")) (T.pack passport)

parse' :: String -> Passport
parse' passport = Passport byr iyr eyr hgt hcl ecl pid cid
  where
    byr =
      case extract "byr" passport of
        Just x -> Just (read x)
        Nothing -> Nothing
    iyr =
      case extract "iyr" passport of
        Just x -> Just (read x)
        Nothing -> Nothing
    eyr =
      case extract "eyr" passport of
        Just x -> Just (read x)
        Nothing -> Nothing
    hgt = extract "hgt" passport
    hcl = extract "hcl" passport
    ecl = extract "ecl" passport
    pid = extract "pid" passport
    cid = extract "cid" passport

parse :: String -> [Passport]
parse contents =
  map (parse' . T.unpack . (T.replace (T.pack "\n") (T.pack " "))) $
  T.splitOn (T.pack "\n\n") (T.pack contents)

isLegal :: Passport -> Bool
isLegal (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) =
  True
isLegal _ = False

partOne :: [Passport] -> Int
partOne passports = length $ filter (== True) $ map isLegal passports

run contents = do
  let input = parse contents
  print $ partOne input
