module Year2020.Day04 where

import Data.Char (isDigit)
import qualified Data.List as L
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
  map (parse' . T.unpack . T.replace (T.pack "\n") (T.pack " ")) $
  T.splitOn (T.pack "\n\n") (T.pack contents)

isLegal :: Passport -> Bool
isLegal (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) =
  True
isLegal _ = False

isLegalHeight :: String -> Bool
isLegalHeight hgt
  | length hgt < 3 = False
  | unit == "cm" = 150 <= number && number <= 193
  | unit == "in" = 59 <= number && number <= 76
  | otherwise = False
  where
    rhgt = reverse hgt
    unit =
      if length hgt < 3
        then ""
        else [rhgt !! 1, head rhgt]
    number = (read :: String -> Int) $ filter isDigit hgt

isLegalHairColor :: String -> Bool
isLegalHairColor hcl =
  length hcl == 7 &&
  head hcl == '#' && all (`elem` "0123456789abcdef") (tail hcl)

isLegalEyeColor :: String -> Bool
isLegalEyeColor ecl =
  ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isLegalStrict :: Passport -> Bool
isLegalStrict (Passport (Just byr) (Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) _) =
  1920 <= byr &&
  byr <= 2002 &&
  2010 <= iyr &&
  iyr <= 2020 &&
  2020 <= eyr &&
  eyr <= 2030 &&
  isLegalHeight hgt &&
  isLegalHairColor hcl &&
  isLegalEyeColor ecl && length pid == 9 && all isDigit pid
isLegalStrict _ = False

partOne :: [Passport] -> Int
partOne passports = length $ filter (== True) $ map isLegal passports

partTwo :: [Passport] -> Int
partTwo passports = length $ filter (== True) $ map isLegalStrict passports

run contents = do
  let input = parse contents
  print $ partOne input
  print $ partTwo input
