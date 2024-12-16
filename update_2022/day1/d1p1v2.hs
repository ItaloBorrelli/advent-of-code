import System.Environment ( getArgs )
import System.IO ()

readInt :: String -> Int
readInt = read

prependToHead :: a -> [[a]] -> [[a]]
prependToHead x [] = [[x]]
prependToHead x (y:ys) = (x:y):ys

split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split [x] dl
    | x == dl = []
    | otherwise = map (:[]) [x]
split (x:y:xs) dl
    | y == dl = [x]:split xs dl
    | otherwise = prependToHead x $ split (y:xs) dl

parse :: String -> [[Int]]
parse s = (map . map) readInt $ (split . lines) s ""

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    print $ maximum (map sum $ parse contents)