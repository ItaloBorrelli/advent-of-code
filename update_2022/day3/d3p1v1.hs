import System.Environment ( getArgs )
import System.IO ()

split :: Int -> [a] -> ([a], [a])
split _ [] = ([], [])
split n (x:xs)
    | n == 0 = ([x], xs)
    | otherwise = (x:xs1, xs2)
    where (xs1, xs2) = split (n-1) xs

splitInHalf :: [a] -> ([a], [a])
splitInHalf x = split n x
    where n = div (length x) 2 - 1

isIn :: Eq a => a -> [a] -> Bool
isIn _ [] = False
isIn x (y:ys)
    | x == y = True
    | otherwise = isIn x ys

findMatch :: ([Char], [Char]) -> Char
findMatch ([], _) = 'a'
findMatch (x:xs, y)
    | x `isIn` y = x
    | otherwise = findMatch (xs, y)

toPriority :: Char -> Int
toPriority x
    | val >= 97 = val - 96
    | otherwise = val - 38
    where val = fromEnum x

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let lineList = lines contents
    print $ sum $ map (toPriority . findMatch . splitInHalf) lineList