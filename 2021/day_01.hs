-- import System.IO
-- import qualified Data.Text as T
-- import qualified Data.Text.Read
import Data.Char

buildPairs [] = []
buildPairs [x] = []
buildPairs (x:y:ys) = (x,y) : buildPairs (y:ys)

buildTriples [] = []
buildTriples [x] = []
buildTriples [x,y] = []
buildTriples (x:y:z:zs) = (x,y,z) : buildTriples (y:z:zs)

isIncreased :: [(Int,Int)] -> [Int]
isIncreased = map (\ x -> if uncurry (<) x then 1 else 0)

sumTriples (x,y,z) = x + y + z

partOne x = sum $ (isIncreased . buildPairs) x

partTwo x = map sumTriples (buildTriples x)

readInt :: String -> Int
readInt = read

main = do
    contents <- readFile "../2021/data/input_01.txt"
    let input = map readInt . words $ contents
    print $ partOne input

    print $ (partOne . partTwo) input

