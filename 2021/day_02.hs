readInt :: String -> Int
readInt = read

parse [x,y] = (x,(readInt y))

horizontal (x,y) = if x == "forward" then y else 0

depth (x,y) = case x of
    "down" -> y
    "up" -> -y
    _ -> 0

aim x = scanl1 (+) (map (depth) x)

multiply (x,y) = x * y

depthTwo x = map (multiply) $ zip (map (horizontal) x) (aim x)

partOne x = (*) (sum $ map (horizontal) x) (sum $ map (depth) x)

partTwo x = (*) (sum $ map (horizontal) x) ((sum . depthTwo) x)

main = do
    contents <- readFile "../2021/data/input_02.txt"
    let input = map (parse) $ map (words) $ lines contents
    print $ partOne input

    print $ partTwo input

