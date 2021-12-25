module Year2016.Day03 where
import qualified Data.List as L
import qualified Data.Text as T

triplets :: Bool -> [String] -> [(Int, Int, Int)]
triplets False (a:b:c:xs) = (read a, read b, read c):triplets False xs
triplets True (a:b:c:d:e:f:g:h:i:xs) = (read a, read d, read g):(read b, read e, read h):(read c, read f, read i):triplets True xs
triplets _ _ = []

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a,b,c) = d + e > f
  where [d,e,f] = L.sort [a,b,c]

parse :: Bool -> String -> [(Int, Int, Int)]
parse part_two = triplets part_two . words

countTriangles :: [(Int, Int, Int)] -> Int
countTriangles candidates = length $ filter (==True) $ map isTriangle candidates

run :: String -> IO ()
run contents = do
  print $ countTriangles $ parse False contents
  print $ countTriangles $ parse True contents
