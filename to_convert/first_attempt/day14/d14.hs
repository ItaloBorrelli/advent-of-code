import Data.Array (array)
import Data.Bifunctor (bimap, first, second)
import Data.List (find)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP (char, eof, pfail, readP_to_S, readS_to_P, sepBy, sepBy1, string, ReadP)

type C = (Int, Int)
type R = [C]
type Map = (Int, a)

mapReadP :: ReadP [Int] -> ReadP (Int, Int)
mapReadP p = do
  xs <- p
  case xs of
    [] -> pfail
    (x:y:_) -> return (x, y)
    _ -> pfail

c :: ReadP C
c = mapReadP $ readS_to_P reads `sepBy1` char ','

r :: ReadP R
r = c `sepBy` string " -> " <* eof

isBetweenInt :: Int -> Int -> Int -> Bool
isBetweenInt q1 q2 q3 = q1 <= q3 && q2 >= q3 || q1 >= q3 && q2 <= q3

isBetweenC :: C -> C -> C -> Bool
isBetweenC (x1, y1) (x2, y2) (x3, y3)
    | x1 == x2 = x1 == x3 && isBetweenInt y1 y2 y3
    | otherwise = y1 == y3 && isBetweenInt x1 x2 x3

inWall :: [C] -> C -> Bool
inWall (_:[]) _ = False
inWall (c1:c2:cs) p = isBetweenC c1 c2 p || inWall (c2:cs) p

isWall :: [[C]] -> C -> Bool
isWall [] _ = False
isWall (w:ws) p = inWall w p || isWall ws p

blocked :: R -> [R] -> C -> Bool
blocked bs ws p = (snd p) >= (getVal bs (fst p)) || isWall ws p

getVal :: [Map] -> Int -> Int
getVal bs x = snd . fromJust $ find (\b -> fst b == x) bs

setVal :: [Map] -> Int -> Int -> [Map]
setVal ms x y = uncurry (++) $ (\k -> (fst k, (x,y):(tail $ snd k))) $ span (\q -> fst q /= x) ms

dropS :: (Int, Int) -> C -> [Map] -> [R] -> ([Map], Bool)
dropS (xMin, xMax) c bs ws
    | cx <= xMin || cx >= xMax = (bs, True)
    | blockedDown && blockedLeft && blockedRight = (setVal bs cx cy, False)
    | blockedDown && blockedLeft = dropS (xMin, xMax) (cx+1, cy+1) bs ws
    | blockedDown = dropS (xMin, xMax) (cx-1, cy+1) bs ws
    | otherwise = dropS (xMin, xMax) (cx, cy+1) bs ws
    where ((cx, cy), blockedDown, blockedLeft, blockedRight) =
            (c, blocked bs ws (second (+1) c),
            blocked bs ws (bimap (flip (-)1) (+1) c),
            blocked bs ws (bimap (+1) (+1) c))

solve :: Int -> (Int, Int) -> [R] -> [Map] -> Int
solve count (xMin, xMax) ws bs
    | done = count
    | otherwise = solve (count+1) (xMin, xMax) ws bsNew
    where (bsNew, done) = dropS (xMin, xMax) (500, 0) bs ws

main :: IO ()
main = do
    contents <- readFile "./input"
    let ws = map (fst . flip (!!) 0 . readP_to_S r) $ lines contents
    let ((xMin, xMax), yMax) = (\q -> ((\xs -> (minimum xs, maximum xs)) $ map fst q, maximum $ map snd q)) $ foldr (++) [] ws
    let bs = (zip [xMin..xMax] $ replicate xMax yMax) :: [Map]
    print $ solve 0 (xMin, xMax) ws bs