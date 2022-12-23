import Data.List (mapAccumL, transpose)
import Data.Bifunctor (bimap, first)
import Data.Tuple (swap)

readInt :: Char -> Int
readInt = read . (:[])

checkRow :: Int -> [(Int, Int)] -> ([(Int, Int)], Int)
checkRow _ [] = ([], 0)
checkRow prevMax (x:xs)
    | prevMax >= fst x = first (x:) $ checkRow h xs
    | snd x == 1 = first (x:) $ checkRow h xs
    | otherwise = bimap ((h, 1):) (+1) $ checkRow h xs
    where h = max prevMax $ fst x

visit :: [[(Int, Int)]] -> ([[(Int, Int)]], Int)
visit = swap . mapAccumL (\x y ->
    let new = checkRow (-1) y
    in (x + snd new, fst new)
    ) 0

main :: IO ()
main = do
    contents <- readFile "./input"
    let lineList = lines contents
    let treeMap = (map . map) (\x -> (readInt x, 0)) lineList
    let (a, ca) = visit treeMap
    let (b, cb) = visit $ map reverse a
    let (c, cc) = visit $ transpose b
    let (d, cd) = visit $ map reverse c
    print (ca + cb + cc + cd)