import System.Environment ( getArgs )
import System.IO ()

splitAtDelimiter :: Eq a => a -> [a] -> ([a], [a])
splitAtDelimiter _ [] = ([], [])
splitAtDelimiter d (x:xs)
    | x == d = ([], xs)
    | otherwise =
        (x:r, s)
        where (r, s) = splitAtDelimiter d xs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

readInt :: [Char] -> Int
readInt = read

convertToRange :: [Char] -> ((Int, Int), (Int, Int))
convertToRange = mapTuple (mapTuple readInt . splitAtDelimiter '-') . splitAtDelimiter ','

hasOverlap :: ((Int, Int), (Int, Int)) -> Bool
hasOverlap ((x1, x2), (y1, y2)) =
    (x1 >= y1 && x2 <= y2)
    || (x1 <= y1 && x2 >= y2)
    || (x1 <= y2 && x2 >= y1)
    || (x2 >= y1 && x1 <= y2)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let lineList = lines contents
    print $ length $ filter id $ map (hasOverlap . convertToRange) lineList