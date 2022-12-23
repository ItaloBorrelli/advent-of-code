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
    | h <= 0 = 0
    | length l < h || x == 0 = getViewPastTree l h-1
    | otherwise = x
    where x = l !! (h-1)

setViewPastTree :: [Int] -> Int -> [Int]
setViewPastTree l 0 = []
setViewPastTree l h = setViewPastTree l (h-1) ++ [getViewPastTree l h + 1]

getView :: Int -> [Int] -> [(Int, View)] -> [(Int, View)]
getView _ _ [] = []
getView _ _ (x:[]) = [x]
getView dir l (x:y:xs)
    | xH >= yH = x:(yH, updateView yV dir 1):rest
    | otherwise = x:(yH, updateView yV dir (1 + getViewPastTree l yH)):rest
    where ((xH, xV), (yH, yV), rest) = (x, y, getView dir (setViewPastTree l $ fst y) xs)

getViews :: Int -> [[(Int, View)]] -> [[(Int, View)]]
getViews dir = map (getView dir [])

getMaxScore :: [[(Int, View)]] -> [Int]
getMaxScore = map (score . snd) . concat

main :: IO ()
main = do
    contents <- readFile "./testinput"
    let lineList = lines contents
    let treeMap = (map . map) (\x -> (readInt x, zeroView)) lineList
    let a = getViews 0 treeMap
    let b = getViews 1 (map reverse a)
    let c = getViews 2 (transpose b)
    let d = getViews 3 (map reverse c)
    print $ map reverse $ transpose $ map reverse d
    print $ getMaxScore d