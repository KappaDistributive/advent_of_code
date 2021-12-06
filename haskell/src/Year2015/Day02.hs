module Year2015.Day02 (run) where
import qualified Data.Text as T
import qualified Data.List as L

data Box = Box Int Int Int
  deriving (Eq, Show)

createBox :: [T.Text] -> Box
createBox [l,w,h] = Box (read (T.unpack l)) (read (T.unpack w)) (read (T.unpack h))
createBox _ = Box (-1) (-1) (-1)

parse :: String -> [Box]
parse x = map (createBox . T.splitOn (T.pack "x") . T.pack) (lines x)

surface :: Box -> Int
surface (Box l w h) = 2*l*w + 2*w*h + 2*h*l

slack :: Box -> Int
slack (Box l w h) = a*b
  where sorted = L.sort [l,w,h]
        [a,b,_] = sorted

ribbon :: Box -> Int
ribbon (Box l w h) = minimum [2*(l+w), 2*(l+h), 2*(w+h)]

bow:: Box -> Int
bow (Box l w h) = l*w*h

ribbonAndBow :: Box -> Int
ribbonAndBow x = ribbon x + bow x

paper :: Box -> Int
paper x = surface x + slack x

partOne :: String -> Int
partOne = sum . map paper . parse

partTwo :: String -> Int
partTwo = sum . map ribbonAndBow . parse

run contents = do
  print $ partOne contents
  print $ partTwo contents
