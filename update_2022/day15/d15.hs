import Control.Applicative ((<|>))
import Data.Bifunctor (bimap, first, second)
import Data.Maybe (catMaybes)
import Data.List (concatMap, sortBy)
import Text.ParserCombinators.ReadP
    ( char
    , eof
    , many
    , pfail
    , readP_to_S
    , readS_to_P
    , satisfy
    , sepBy1
    , string
    , (+++)
    , ReadP)

type C      = (Int, Int)
type SpanX  = (Int, Int)
type SpanY  = (Int, Int)

data Edge   = Edge      { m :: Int
                        , b :: Int
                        , spanX :: SpanX }  deriving (Eq, Show)

data EdgeM  = EdgeM     { mm :: Int
                        , bm :: Int
                        , spanXs :: [SpanX] }  deriving (Eq, Show)

data Bounds = Bounds    { tr :: Edge
                        , br :: Edge
                        , bl :: Edge
                        , tl :: Edge }      deriving (Show)

mapReadP :: ReadP [a] -> ReadP (a, a)
mapReadP p = do
    xs <- p
    case xs of
        [] -> pfail
        (x:y:_) -> return (x, y)
        _ -> pfail

readC :: ReadP C
readC = mapReadP $ readS_to_P reads `sepBy1` string ", y="

readB :: ReadP (C, C)
readB = mapReadP $ (many $ string "Sensor at x=" +++ string ": closest beacon is at x=" *> readC) <* eof

constructBounds :: Int -> (C, C) -> Bounds
constructBounds addD ((x0, y0), (x1, y1)) =
    let (md, od) =
            ( abs (x0-x1)   + abs (y0-y1)   + addD
            , abs (x0-0 )   + abs (y0-0 )           )
    in  Bounds
        (Edge   (-1)    (od+md)         (x0     , x0+md ))
        (Edge   ( 1)    (od-md-(x0*2))  (x0     , x0+md ))
        (Edge   (-1)    (od-md)         (x0-md  , x0    ))
        (Edge   ( 1)    (od+md-(x0*2))  (x0-md  , x0    ))

xtoy :: Int -> Int -> Int -> Int
xtoy x m b = m * x + b

ytox :: Int -> Int -> Int -> Int
ytox y m b = (y - b) `div` m

ytox' :: Double -> Double -> Double -> Double
ytox' y m b = (y - b) / m

intersect :: SpanX -> SpanX -> Maybe SpanX
intersect (lba, uba) (lbb, ubb)
    | lba > uba || lbb > ubb || uba < lbb || ubb < lba = Nothing
    | otherwise = Just (max lba lbb, min uba ubb)

inBound :: Int -> SpanX -> Bool
inBound x (m, n) = m <= x && n >= x

getXRange :: Int -> (Edge, Edge) -> Maybe SpanX
getXRange y (l, r) =
    (\(xl, xr) -> if xl `inBound` (spanX l) then Just (xl, xr) else Nothing)
    (ytox y (m l) (b l), ytox y (m r) (b r))

getXRangeOfBound :: Int -> Bounds -> Maybe SpanX
getXRangeOfBound y b = getXRange y (tl b, tr b) <|> getXRange y (bl b, br b)

sortSpans :: [SpanX] -> [SpanX]
sortSpans = sortBy (\(t0, _) -> \(t1, _) -> compare t0 t1)

squash :: [SpanX] -> [SpanX]
squash ((fr0, br0):(fr1, br1):rs)
    | (br0+1) >= fr1 = if br1 >= br0 then squash $ (fr0, br1):rs else squash $ (fr0, br0):rs
    | otherwise = (fr0, br0):(squash $ (fr1, br1):rs)
squash rs = rs

tune :: Int -> Int -> Int
tune x y = 4000000 * x + y

intersection :: Int -> Int -> Int -> Int
intersection m0 b0 b1 = (\y -> ytox y m0 b0) $ ((b0) + (b1)) `div` 2

intersection' :: Int -> Int -> Int -> Double
intersection' m0 b0 b1 = (\y -> ytox' y (fromIntegral m0) (fromIntegral b0)) $ (fromIntegral b0 + fromIntegral b1) / 2.0

check :: C -> Maybe C
check (a, b) = if b < a then Nothing else Just (a, b)

removeBad :: [SpanX] -> [SpanX]
removeBad = filter (\(a, b) -> a <= b)

boundVals :: SpanX -> SpanY -> Int -> Int -> [SpanX] -> [SpanX]
boundVals _ _ _ _ [] = []
boundVals (xMin, xMax) (yMin, yMax) mI bI ((a, b):ss) =
    let (xFromYMin, xFromYMax) = ((yMin - bI) `div` mI, (yMax - bI) `div` mI)
    in (
      maximum [xMin, if mI == 1     then xFromYMin  else xFromYMax, a]
    , minimum [xMax, if mI == -1    then xFromYMin  else xFromYMax, b])
    :boundVals (xMin, xMax) (yMin, yMax) mI bI ss

splitOnBound :: Bounds -> Int -> Int -> SpanX -> [SpanX]
splitOnBound bound mI bI span
    | mI == 1 = if bI < b brE || bI > b tlE then [span] else
        [ (max (fst span) $ (floor $ intersection' mI bI (b trE)) + 1, snd span)
        , (fst span, min (snd span) $ (ceiling $ intersection' mI bI (b blE)) - 1) ]
    | otherwise = if bI < b blE || bI > b trE then [span] else
        [ (fst span, min (snd span) $ (ceiling $ intersection' mI bI (b tlE)) - 1)
        , (max (fst span) $ (floor $ intersection' mI bI (b brE)) + 1, snd span) ]
    where (trE, brE, blE, tlE) = (tr bound, br bound, bl bound, tl bound)

updateSpan :: SpanX -> SpanY -> Bounds -> Int -> Int -> [SpanX] -> [SpanX]
updateSpan sx sy bound mI bI spans =
    (removeBad . (boundVals sx sy mI bI) . squash . sortSpans . removeBad . concatMap (splitOnBound bound mI bI)) spans

cutEdge :: SpanX -> SpanY -> Bounds -> EdgeM -> EdgeM
cutEdge sx sy bound i = EdgeM (mI) (bI) (updateSpan sx sy bound mI bI $ spanXs i)
    where (mI, bI) = (mm i, bm i)

solve1 :: Int -> [Bounds] -> Int
solve1 y bs = sum $ map (\(a, b) -> b - a) $ (squash . sortSpans) $ catMaybes $ map (getXRangeOfBound y) bs

solve2 :: SpanX -> SpanY -> [Bounds] -> [EdgeM] -> Int
solve2 sx sy bs is =
    (\e -> (\x -> tune x (xtoy x (mm e) (bm e))) $ (fst . flip (!!) 0 . spanXs) e)
    $ (flip (!!) 0)
    $ filter (\e -> length (spanXs e) /= 0)
    $ map (\i -> foldr (\b -> \newI -> cutEdge sx sy b newI) i bs) is

main :: IO ()
main = do
    contents <- readFile "./input"
    let bs = map (constructBounds 0 . fst . flip (!!) 0 . readP_to_S readB) $ lines contents
    let is =
            map (\e -> EdgeM (m e) (b e) [spanX e])
            $ concatMap (\b -> [tr b, br b, bl b, tl b])
            $ map (constructBounds 1 . fst . flip (!!) 0 . readP_to_S readB)
            $ lines contents
    print "part1"
    -- print $ solve1 10 bs
    print $ solve1 2000000 bs
    print "part2"
    -- print $ solve2 (0, 20) (0, 20) bs is
    print $ solve2 (0, 4000000) (0, 4000000) bs is