import Data.Bifunctor (bimap, first, second)
import Data.List (find, sortBy)
import Text.ParserCombinators.ReadP (
    char
    , eof
    , pfail
    , readP_to_S
    , readS_to_P
    , sepBy
    , sepBy1
    , string
    , ReadP)

type C = (Int, Int)
type W = [C]

type R = (Int, Int)
type M = [(Int, [R])]

mapReadP :: ReadP [Int] -> ReadP (Int, Int)
mapReadP p = do
    xs <- p
    case xs of
        [] -> pfail
        (x:y:_) -> return (x, y)
        _ -> pfail

c :: ReadP C
c = mapReadP $ readS_to_P reads `sepBy1` char ','

w :: ReadP W
w = c `sepBy` string " -> " <* eof

split :: Int -> [R] -> ([R], [R])
split y = span (\(t, b) -> t < y && b < y)

splitMap :: Int -> M -> (M, M)
splitMap x0 = span (\(x1, _) -> x0 /= x1)

isBlocked :: Int -> [R] -> (Bool, ([R], [R]))
isBlocked y rs =
    (\(above, below) -> if length below == 0 then (False, ([], [])) else (y >= (fst . head) below, (above, below)))
    $ split y rs

fall :: C -> R -> Int -> M -> (Bool, M)
fall (x, y) (xMin, xMax) yMax m
    | x <= xMin || x >= xMax = (True, [])
    | y >= yMax = (True, [])
    | blockedDown && blockedLeft && blockedRight = if (x, y) == (500, 0) then (True, []) else
        (\(q, r) -> (False
        , splitAtXFront
        ++ (x, (squash . sortRs) (above
        ++ (first (\_ -> y)
        $ head below)
        :tail below))
        :tail splitAtXBack))
        $ splitMap x m
    | blockedDown && blockedLeft = fall (x+1, y+1) (xMin, xMax) yMax m
    | blockedDown = fall (x-1, y+1) (xMin, xMax) yMax m
    | otherwise = fall (x, y+1) (xMin, xMax) yMax m
    where ( (splitAtXFront, splitAtXBack)
            , (blockedDown, (above, below))
            , (blockedLeft, _)
            , (blockedRight, _)
            ) = (
            splitMap x m
            , isBlocked (y+1) $ (snd . head . snd) $ splitMap x m
            , isBlocked (y+1) $ (snd . head . snd) $ splitMap (x-1) m
            , isBlocked (y+1) $ (snd . head . snd) $ splitMap (x+1) m)

solve1 :: Int -> (Int, Int) -> Int -> M -> Int
solve1 count (xMin, xMax) yMax m
    | done = count
    | otherwise = solve1 (count+1) (xMin, xMax) yMax newM
    where (done, newM) = fall (500, 0) (xMin, xMax) yMax m

solve2 :: Int -> M -> Int
solve2 count m
    | done = count
    | otherwise = solve2 (count+1) newM
    where (done, newM) = fall (500, 0) (0, 1000) 1000 m

addAtY :: Int -> (Int, [R]) -> (Int, [R])
addAtY y (x, rs) = (x, (\(top, bottom) -> top ++ (y, y):bottom) $ split y rs)

wallToMap :: C -> C -> M -> M
wallToMap (xF, yF) (xT, yT) m
    | xF == xT = if yF > yT then wallToMap (xT, yT) (xF, yF) m else
        (\((front, back)) ->
            front
            ++ ((\(top, bottom) ->
                (xF, top ++ (yF, yT):bottom))
                $ (split yF $ (snd . head) back))
            :tail back)
        $ splitMap xF m
    | otherwise = if xF > xT then wallToMap (xT, yT) (xF, yF) m else
        (\(front, back) -> front ++ ((\(b0, b1) -> (map (addAtY yF) b0 ++ b1)) $ splitMap (xT+1) back)) $ splitMap xF m

wallsToMap :: W -> M -> M
wallsToMap (w0:w1:ws) m = wallToMap w0 w1 $ wallsToMap (w1:ws) m
wallsToMap _ m = m

sortRs :: [R] -> [R]
sortRs = sortBy (\(t0, _) -> \(t1, _) -> compare t0 t1)

squash :: [R] -> [R]
squash ((fr0, br0):(fr1, br1):rs)
    | (br0+1) >= fr1 = if br1 >= br0 then squash $ (fr0, br1):rs else squash $ (fr0, br0):rs
    | otherwise = (fr0, br0):(squash $ (fr1, br1):rs)
squash rs = rs

main :: IO ()
main = do
    contents <- readFile "./testinput"
    let ws1 = map (fst . flip (!!) 0 . readP_to_S w) $ lines contents
    let ((xMin, xMax), yMax) =
            (\q -> ((\xs -> (minimum xs, maximum xs)) $ map fst q, maximum $ map snd q))
            $ foldr (++) [] ws1
    let ws2 = [(0, yMax + 2), (1000, yMax + 2)]:ws1
    let m1 = map (\(x, rs) -> (x, (squash . sortRs) rs)) $ foldr wallsToMap (map (\x -> (x, [])) [xMin..xMax]) ws1
    let m2 = map (\(x, rs) -> (x, (squash . sortRs) rs)) $ foldr wallsToMap (map (\x -> (x, [])) [0..1000]) ws2
    print "part1"
    print $ solve1 0 (xMin, xMax) yMax m1
    print "part2"
    print $ solve2 1 m2