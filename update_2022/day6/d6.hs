import System.Environment ( getArgs )
import System.IO ()
import Control.Arrow (first)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

allUnique :: [Char] -> Bool
allUnique [] = True
allUnique (x:xs) = if' (x `notElem` xs) (allUnique xs) False

splitAtIndex :: Int -> [Char] -> ([Char], [Char])
splitAtIndex 0 xs = ([], xs)
splitAtIndex _ [] = error "Not enough values"
splitAtIndex idx (x:xs) = first (x:) $ splitAtIndex (idx-1) xs

getMarker :: Int -> ([Char], [Char]) -> Int
getMarker idx (x, y)
    | allUnique x = idx
    | otherwise = getMarker (idx+1) (tail x ++ [head y], tail y)

main :: IO ()
main = do
    contents <- readFile "./input"
    let lineList = lines contents
    print "part 1"
    print $ head $ map (getMarker 4 . splitAtIndex 4) lineList
    print "part 2"
    print $ head $ map (getMarker 14 . splitAtIndex 14) lineList