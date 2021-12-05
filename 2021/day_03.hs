import Data.Array

parse :: String -> [[Bool]]
parse x = map decode (lines x)

decode :: String -> [Bool]
decode = map(== '1')

decodeBinary' :: [Int] -> Int
decodeBinary' [] = 0
decodeBinary' (x:xs) = x + 2 * decodeBinary' xs

decodeBinary :: [Int] -> Int
decodeBinary = decodeBinary' . reverse

position :: (Int, [[a]]) -> [a]
position (pos, x:xs) = (x !! pos) : position (pos, xs)
position (_, _) = []

countOnes :: Int -> [[Bool]] -> Int
countOnes pos codes = sum $ map fromEnum (position (pos, codes))

countAllOnes :: [[Bool]] -> [Int]
countAllOnes x = map (`countOnes` x) [0,1.. length (head x) - 1]

gamma :: [[Bool]] -> Int
gamma x = decodeBinary $ map (\n -> if 2 * n > length x then 1 else 0) (countAllOnes x)

epislon :: [[Bool]] -> Int
epislon x = decodeBinary $ map (\n -> if 2 * n < length x then 1 else 0) (countAllOnes x)
 
partOne x = gamma x * epislon x

main = do
  contents <- readFile "../2021/data/input_03.txt"
  let input = parse contents
  print $ partOne input
