import Data.List (concat, mapAccumL, transpose)
import Data.Bifunctor (bimap, first)
import Data.Tuple (swap)

readInt :: Char -> Int
readInt = read . (:[])

data View = V { sights :: (Int, Int, Int, Int), score :: Int } deriving (Show)
zeroView :: View
zeroView = V (0, 0, 0, 0) 0

newView :: (Int, Int, Int, Int) -> View
newView (a, b, c, d) = V (a, b, c, d) (a * b * c * d)

updateView :: View -> Int -> Int -> View
updateView v dir val
    | dir == 0 = newView (val, b, c, d)
    | dir == 1 = newView (a, val, c, d)
    | dir == 2 = newView (a, b, val, d)
    | otherwise = newView (a, b, c, val)
    where (a, b, c, d) = sights v

getViewPastTree :: [Int] -> Int -> Int
getViewPastTree [] _ = 0
getViewPastTree l h
    | h < 0 = 0
    | length l <= h || x == 0 = getViewPastTree l (h-1)
    | otherwise = x
    where x = l !! h

setViewPastTree :: [Int] -> Int -> [Int]
setViewPastTree l h = map (\x ->
    if x <= h then 1
    else (getViewPastTree l x) + 1
    ) [0..9]

getView :: Int -> [Int] -> [(Int, View)] -> [(Int, View)]
getView _ _ [] = []
getView _ _ (x:[]) = [x]
getView dir l (x:y:xs) =
    let ((xH, xV), (yH, yV), rest) = (x, y, getView dir (setViewPastTree l yH) $ y:xs)
    in x:(getView dir (setViewPastTree l yH) $ (yH, updateView yV dir $ getViewPastTree l yH):xs)

getViews :: Int -> [[(Int, View)]] -> [[(Int, View)]]
getViews dir = map (getView dir [1])

getMaxScore :: [[(Int, View)]] -> Int
getMaxScore x = maximum . map (score . snd) $ concat x

main :: IO ()
main = do
    contents <- readFile "./input"
    let lineList = lines contents
    let treeMap = (map . map) (\x -> (readInt x, zeroView)) lineList
    let a = getViews 0 treeMap
    let b = getViews 1 (map reverse a)
    let c = getViews 2 (transpose b)
    let d = getViews 3 (map reverse c)
    print $ getMaxScore d