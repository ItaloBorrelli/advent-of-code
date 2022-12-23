import System.Environment ( getArgs )
import System.IO ()
import Data.Maybe

isIn :: Eq a => a -> [a] -> Bool
isIn _ [] = False
isIn x (y:ys)
    | x == y = True
    | otherwise = isIn x ys

findMatch :: Eq a => [[a]] -> Maybe a
findMatch [x:xs] = Just x
findMatch ((x:xs):(y:ys):rest)
    | x == y =
        let match = findMatch ((y:ys):rest)
        in
            if isNothing match
            then findMatch (xs:((y:ys):rest))
            else match
    | otherwise =
        let match = findMatch ([x]:(ys:rest))
        in
            if isNothing match
            then findMatch (xs:((y:ys):rest))
            else match
findMatch _ = Nothing

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n list
    | n > 0 = take n list : chunk n (drop n list)
    | otherwise = error ""

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
    let splitList = chunk 3 lineList
    print $ sum $ map (toPriority . fromJust . findMatch) splitList