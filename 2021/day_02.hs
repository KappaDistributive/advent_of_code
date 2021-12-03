
readInt :: String -> Int
readInt = read

parse [x,y] = (x,(readInt y))

horizontal (x,y) = if x == "forward" then y else 0
depth (x,y) = case x of
    "down" -> y
    "up" -> -y
    _ -> 0

partOne x = (*) (sum $ map (horizontal) x) (sum $ map (depth) x)

main = do
    contents <- readFile "../2021/data/input_02_mock.txt"
    let input = map (parse) $ map (words) $ lines contents
    print $ partOne input
