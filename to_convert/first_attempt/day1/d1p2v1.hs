import System.Environment ( getArgs )
import System.IO ()

max' :: [Int] -> Int
max' [] = 0
max' [x] = x
max' (x:xs)
    | x > y = x
    | otherwise = y
    where y = max' xs

delete' :: Eq a => [a] -> a -> [a]
delete' [] _ = []
delete' (x:xs) r
    | x == r = xs
    | otherwise = x:delete' xs r

maxOf :: [Int] -> Int -> [Int]
maxOf _ 0 = []
maxOf [] _ = error "Not enough values in list"
maxOf x c = m:maxOf (delete' x m) (c-1)
    where m = max' x


readInt :: String -> Int
readInt = read

prependToHead :: a -> [[a]] -> [[a]]
prependToHead x [] = [[x]]
prependToHead x (y:ys) = (x:y):ys

split' :: Eq a => [a] -> a -> [[a]]
split' [] _ = []
split' [x] d
    | x == d = []
    | otherwise = map (:[]) [x]
split' (x:y:xs) d
    | y == d = [x]:split' xs d
    | otherwise = prependToHead x $ split' (y:xs) d

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let lineList = lines contents
    let inputArray = map (map readInt) $ split' lineList ""
    print (sum (maxOf (map sum inputArray) 3))