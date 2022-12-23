import System.Environment ( getArgs )
import System.IO ()
import Data.List (transpose)
import Control.Arrow (second)

getHeight :: [[Char]] -> Int
getHeight [] = 0
getHeight (x:xs)
    | (head . tail) x == '1' = 0
    | otherwise = 1 + getHeight xs

getValInNthPos :: Int -> [a] -> a
getValInNthPos _ [] = error "Not enough values to get nth"
getValInNthPos 0 (x:xs) = x
getValInNthPos n (x:xs)
    | n > 0 = getValInNthPos (n-1) xs
    | otherwise = error "Can't get negative value"

getNthOfEachStack :: Int -> Int -> [Char] -> [Char]
getNthOfEachStack _ _ [] = []
getNthOfEachStack perStack n row =
    getValInNthPos n (take perStack row) : getNthOfEachStack perStack n (drop perStack row)

splitAtNSkipM :: Int -> Int -> [a] -> ([a], [a])
splitAtNSkipM _ _ [] = ([], [])
splitAtNSkipM 0 0 xl = ([], xl)
splitAtNSkipM m 0 (x:xs) = splitAtNSkipM (m-1) 0 xs
splitAtNSkipM m n (x:xs) =
    let (front, back) = splitAtNSkipM m (n-1) xs
    in (x:front, back)

removeFrontWhitespace :: [Char] -> [Char]
removeFrontWhitespace [] = []
removeFrontWhitespace (x:xs)
    | x == ' ' = removeFrontWhitespace xs
    | otherwise = x:xs

splitAtDelimiter :: Eq a => a -> [a] -> ([a], [a])
splitAtDelimiter _ [] = ([], [])
splitAtDelimiter d (x:xs)
    | x == d = ([], xs)
    | otherwise =
        (x:r, s)
        where (r, s) = splitAtDelimiter d xs

getBeforeSpace :: [Char] -> [Char]
getBeforeSpace x = fst $ splitAtDelimiter ' ' x

getAfterSpace :: [Char] -> [Char]
getAfterSpace x = snd $ splitAtDelimiter ' ' x

readInt :: [Char] -> Int
readInt = read

getNumber :: [Char] -> (Int, [Char])
getNumber x =
    let postSpace = getAfterSpace x
    in ((readInt . getBeforeSpace) postSpace, getAfterSpace postSpace)

getInstructions :: [Char] -> (Int, Int, Int)
getInstructions inst =
    let first = getNumber inst
    in let second = getNumber (snd first)
    in (fst first, fst second, fst $ getNumber (snd second))

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i = uncurry (++) . second (\(x:xs) -> f x : xs) . splitAt i

operate :: [[Char]] -> (Int, Int, Int) -> [[Char]]
operate l (num, from, to) =
    let moving = reverse $ take num (l !! max 0 (from-1))
    in adjust (moving ++) (to-1) $ adjust (drop num) (from-1) l

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let lineList = lines contents
    let (stacks, inst) = splitAtNSkipM 2 (getHeight lineList) lineList
    let stackList = map removeFrontWhitespace $ transpose $ map (getNthOfEachStack 4 1) stacks
    let instList = map getInstructions inst
    let finalForm = foldl operate stackList instList
    print $ map head finalForm