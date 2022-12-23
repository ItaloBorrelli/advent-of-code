import System.Environment ( getArgs )
import System.IO ()

mapCharToPoints :: Char -> Int
mapCharToPoints x
    | x `elem` ['A', 'X'] = 1
    | x `elem` ['B', 'Y'] = 2
    | otherwise = 3

mapStringToPoints :: [Char] -> [Int]
mapStringToPoints [i, _, j] = map mapCharToPoints [i, j]
mapStringToPoints _ = []

countPoints :: [Int] -> Int
countPoints [x, y]
    | d == 1 = 6 + y
    | d == 0 = 3 + y
    | otherwise = y
    where d = (y - x) `mod` 3
countPoints _ = 0

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    print $ sum $ map (countPoints . mapStringToPoints) (lines contents)