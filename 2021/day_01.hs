-- import System.IO
-- import qualified Data.Text as T
-- import qualified Data.Text.Read
import Data.Char

pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp (x:[]) = []
pairUp (x:y:ys) = (x,y) : pairUp (y:ys)

isIncreased :: [(Int,Int)] -> [Int]
isIncreased [] = []
isIncreased (x:xs) = (if ((fst x) < (snd x)) then 1 else 0) : (isIncreased xs)

partOne x = sum $ (isIncreased . pairUp) x

readInt :: String -> Int
readInt = read

main = do
    contents <- readFile "../2021/data/input_01.txt"
    let input = map readInt . words $ contents
    print $ partOne input

