import Data.List (concat, mapAccumL, transpose)
import Data.Bifunctor (bimap, first)
import Data.Tuple (swap)

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

getDirectionValue :: View -> Int -> Int
getDirectionValue v dir
    | dir == 0 = a
    | dir == 1 = b
    | dir == 2 = c
    | otherwise = d
    where (a, b, c, d) = sights v

readInt :: Char -> Int
readInt = read . (:[])

getViewOfRow :: [(Int, View)] -> [(Int, View)] -> Int -> [(Int, View)]
getViewOfRow [] _ _ = []
getViewOfRow (x:xs) (y:ys) dir
    | yH >= xH = (yH, updateView yV dir $ 1 + getDirectionValue xV dir):rest
    | otherwise = (yH, updateView yV dir 1):rest
    where ((xH, xV), (yH, yV), rest) = (x, y, getViewOfRow xs ys dir)

getViews :: [[(Int, View)]] -> Int -> [[(Int, View)]]
getViews (x:y:[]) _ = [x,y] -- base case is last row and will have score 0 regardless
getViews (x:y:xs) dir = (\newView -> x:newView:(getViews (newView:xs) dir)) $ getViewOfRow x y dir
getViews _ _ = []

getMaxScore :: [[(Int, View)]] -> [Int]
getMaxScore = map (score . snd) . concat

main :: IO ()
main = do
    contents <- readFile "./testinput"
    let lineList = lines contents
    let treeMap = (map . map) (\x -> (readInt x, zeroView)) lineList
    let a = getViews treeMap 0
    let b = getViews (map reverse a) 1
    let c = getViews (transpose b) 2
    let d = getViews (map reverse c) 2
    print d
    print $ getMaxScore d