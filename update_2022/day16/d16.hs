import Data.Array ((!))
import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.Graph (graphFromEdges, Graph, Vertex)
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as S (filter, fromList, insert, map, null, toList, Set)
import Text.ParserCombinators.ReadP
    ( eof, many1, munch1, readP_to_S, readS_to_P, satisfy, sepBy, string, (+++), ReadP)

import Debug.Trace (trace)
debugMap :: Show a => (a -> b) -> [a] -> [b]
debugMap f xs = map (\x -> trace ("f was called with x = " ++ show x) (f x)) xs


type Valve = String
type VT = (Int, Valve, [Valve])

-- eg. "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
vt :: ReadP VT
vt = do
    string "Valve "
    name <- many1 $ satisfy (const True)
    string " has flow rate="
    flowRate <- many1 $ satisfy isDigit
    string "; tunnels lead to valves " +++ string "; tunnel leads to valve "
    destinations <- munch1 (\c -> c /= ',') `sepBy` string ", " <* eof
    return (read flowRate, name, destinations)

insertAll :: Ord a => [a] -> S.Set a -> S.Set a
insertAll xs s = foldl (flip S.insert) s xs

getSignificant :: Graph -> (Vertex -> (Int, Valve, [Valve])) -> Int -> S.Set Vertex -> [(Valve, Int)]
getSignificant g fromVert depth curr = if depth == 0 then [] else
    S.toList
    $ S.map ((\(_, n, _) -> (n, depth)))
    $ S.filter ((\(flow, n, _) -> flow /=0))
    $ S.map (\v -> fromVert v) curr

bfs :: Graph -> (Vertex -> (Int, Valve, [Valve])) -> Int -> S.Set Vertex -> S.Set Vertex -> [(Valve, Int)]
bfs g fromVert depth xs visited = if S.null xs then [] else
    getSignificant g fromVert depth xs
    ++ (uncurry (bfs g fromVert $ depth+1)
    $ foldr
    (\x -> \(next, newVs) ->
        (\xAdj -> (insertAll xAdj next, insertAll xAdj newVs))
        $ filter (\adj -> not $ adj `elem` visited) $ g ! x)
    (S.fromList [], visited) xs)

dist :: [(String, [(String, Int)])] -> String -> String -> Int
dist distMap from to = (snd . fromJust)
        $ find (\(n, _) -> n == to)
        $ (snd . fromJust)
        $ find (\(n, _) -> n == from) distMap

visit :: [(String, [(String, Int)])] -> [(String, Int)] -> [String] -> String -> Int -> Int -> Int
visit ds fs left curr adder minute
    | minute >= 30 = 0
    | length left == 0 = adder + visit [] [] [] "" adder (minute + 1)
    | otherwise =
        maximum
        $ map (\x ->
            (\time ->
                if (time + minute >= 30) then ((30 - minute) * adder) else
                (time * adder)
                + (visit ds fs
                (uncurry (++) $ second tail $ span (\y -> x /= y) left)
                x (adder + ((snd . fromJust) $ find (\(n, _) -> n == x) fs)))
                (time + minute))
            (dist ds curr x + 1))
          left

allCombosPick1 :: Eq a => [a] -> [(a, [a])]
allCombosPick1 xs =
        map (\x -> (
            \(f, b) -> (head b, f ++ tail b)
            )
        $ span (\y -> y /= x) xs) xs

allCombosPick2 :: Eq a => [a] -> [((a, a), [a])]
allCombosPick2 xs =
        concatMap (\x ->
            (\(f, b) ->
                map (\(pick, rest) ->
                    ((head b, pick), f ++ rest))
                $ allCombosPick1 $ tail b)
            $ span (\y -> x /= y) xs) xs

getFlow :: [(Valve, Int)] -> Valve -> Int
getFlow fs v = if v == "AA" then 0 else snd $ fromJust $ find (\f -> fst f == v) fs

visit' :: [(Valve, [(Valve, Int)])] -> [(Valve, Int)] -> Int -> [String] -> ((String, Int), (String, Int)) -> Int -> Int
visit' ds fs minute left ((currA, distA), (currB, distB)) total_flow
    | minute >= 26 = 0
    | length left == 0 = total_flow * (26 - minute)
    | distA > 0 && distB > 0 = total_flow + progress left ((currA, distA - 1), (currB, distB - 1)) total_flow
    | distA > 0 = total_flow + getFlow fs currB
        + (maximum
        $ map (\(v, vs) ->
            progress vs ((currA, distA - 1), (v, dist ds currB v + 1))
            (total_flow + getFlow fs currB))
        $ allCombosPick1 left)
    | distB > 0 = total_flow + getFlow fs currA
        + (maximum
        $ map (\(v, vs) ->
            progress vs ((v, dist ds currA v + 1), (currB, distB - 1))
            (total_flow + getFlow fs currA))
        $ allCombosPick1 left)
    | length left == 1 = total_flow + getFlow fs currA + getFlow fs currB
        + (max (maximum
        $ map (\(v, vs) ->
            progress vs ((currA, distA - 1), (v, dist ds currB v + 1))
            (total_flow + getFlow fs currB))
        $ allCombosPick1 left)
        (maximum
        $ map (\(v, vs) ->
            progress vs ((v, dist ds currA v + 1), (currB, distB - 1))
            (total_flow + getFlow fs currA))
        $ allCombosPick1 left))
    | otherwise = total_flow + getFlow fs currA + getFlow fs currB
        + (maximum
        $ map (\((v1, v2), vs) ->
            max
            (progress vs ((v1, dist ds currA v1 + 1), (v2, dist ds currB v2 + 1))
            (total_flow + getFlow fs currA + getFlow fs currB))
            (progress vs ((v2, dist ds currA v2 + 1), (v1, dist ds currB v1 + 1))
            (total_flow + getFlow fs currA + getFlow fs currB)))
        $ allCombosPick2 left)
    where progress = visit' ds fs (minute + 1)

solve1 :: [(String, [(String, Int)])] -> [(String, Int)] -> Int
solve1 ds fs = visit ds fs (map (\(n, _) -> n) fs) "AA" 0 0

solve2 :: [(String, [(String, Int)])] -> [(String, Int)] -> Int
solve2 ds fs = visit' ds fs 0 (map (\(n, _) -> n) fs) (("AA", 0), ("AA", 0)) 0

main :: IO ()
main = do
    contents <- readFile "./testinput"
    let vs = map (fst . flip (!!) 0 . readP_to_S vt)
            $ lines contents
    let ss = (map (\(fr, n, _) -> (n, fr))
            $ filter (\(fr, n, _) -> fr /= 0) vs)
    let (g, fromVert, toVert) = graphFromEdges vs
    let adjMap = map (\(n, _) -> (\sVert ->
            (n, bfs g fromVert 0 (S.fromList [sVert]) (S.fromList [sVert])))
            $ fromJust $ toVert n)
            $ ("AA", 0):ss
    print "part1"
    print $ solve1 adjMap ss
    print "part2"
    print $ solve2 adjMap ss