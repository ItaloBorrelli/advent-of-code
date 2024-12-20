{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module AOC.Y2024.Day20 (runDay) where

import Data.Bifunctor (first, second)
import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H
import Data.Map.Strict (Map, (!), (!?))
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void (Void)
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

data D = U | D | L | R deriving (Eq, Show, Ord)

type Start = C

type End = C

data X = W' | N' | S | E deriving (Eq, Show)

data Y = W | N deriving (Eq, Show)

type Grid = [[X]]

data Distance a = Dist a | Infinity
    deriving (Show, Eq)

type M = Map C Y

type Input = (Start, End, M)

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseMapLine :: Parser [X]
parseMapLine = many (W' <$ char '#' <|> N' <$ char '.' <|> S <$ char 'S' <|> E <$ char 'E')

start :: Grid -> C
start = head . concatMap (\(y, row) -> [(y, x) | (x, v) <- zip [0 ..] row, v == S]) . zip [0 ..]

end :: Grid -> C
end = head . concatMap (\(y, row) -> [(y, x) | (x, v) <- zip [0 ..] row, v == E]) . zip [0 ..]

toMap :: Grid -> M
toMap = mapFromNestedLists . map (map (\case W' -> W; _ -> N))

inputParser :: Parser Input
inputParser = (\g -> (start g, end g, toMap g)) <$> parseMapLine `sepBy` newline <* eof

----------- PART A&B -----------

instance (Ord a) => Ord (Distance a) where
    (<=) :: Distance a -> Distance a -> Bool
    Infinity <= Infinity = True
    Infinity <= Dist _ = False
    Dist _ <= Infinity = True
    Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

subDist :: (Num a) => Distance a -> Distance a -> Distance a
subDist (Dist x) (Dist y) = Dist (x - y)
subDist _ _ = Dist 0

(!??) :: (Ord k) => Map k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (distanceMap !? key)

type EdgeMap = Map C [(C, Int)]

newtype Graph = Graph
    {edges :: EdgeMap}
    deriving (Show)

data DijkstraState = DijkstraState
    { visitedSet :: Set C
    , distanceMap :: Map C (Distance Int)
    , nodeQueue :: MinPrioHeap (Distance Int) C
    }

findShortestDistance :: Graph -> C -> Map C (Distance Int)
findShortestDistance graph src = processQueue initialState
  where
    initialVisited = S.empty
    initialDistances = M.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> Map C (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
        Nothing -> d0
        Just ((_, node), q1) ->
            if S.member node v0
                then processQueue (ds {nodeQueue = q1})
                else -- Update the visited set

                    let
                        v1 = S.insert node v0
                        -- Get all unvisited neighbors of our current node
                        allNeighbors = fromMaybe [] (M.lookup node (edges graph))
                        unvisitedNeighbors = filter (\(n, _) -> not (S.member n v1)) allNeighbors
                     in
                        -- Fold each neighbor and recursively process the queue
                        processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
        let
            altDistance = addDist (d0 !?? current) (Dist cost)
         in
            if altDistance < d0 !?? neighborNode
                then DijkstraState v1 (M.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
                else ds

step :: D -> C -> C
step U = second (\x -> x - 1)
step R = first (1 +)
step D = second (1 +)
step L = first (\x -> x - 1)

makeEdges :: C -> [C] -> [(C, (C, Int))]
makeEdges _ [] = []
makeEdges c (x : xs) = (c, (x, 1)) : rest
  where
    rest = makeEdges c xs

constructEdges :: M -> C -> EdgeMap -> EdgeMap
constructEdges m c prev = foldl insertEdge prev moreEdges
  where
    adjs = map fromJust $ filter (/= Nothing) $ map (\d -> let c' = step d c in case m !? c' of Just N -> Just c'; _ -> Nothing) [U, R, L, D]
    moreEdges = makeEdges c adjs
    insertEdge acc (k, v) = M.insertWith (++) k [v] acc

cheats :: C -> [C]
cheats c = [step U (step U c), step L (step L c), step R (step R c), step D (step D c)]

----------- PART A -------------

timeSaver :: M -> Map C (Distance Int) -> C -> Distance Int
timeSaver m d0 c =
    let
        allSkips = filter (\c' -> m !? c' == Just N) $ cheats c
     in
        maximum (Dist 0:map (\s -> subDist (d0 ! c) (fromMaybe Infinity (d0 !? s))) allSkips)

partA :: Input -> Int
partA (s, e, m) =
    ( \m' ->
            length $
            filter
                ( \(c, f) -> case f of
                    Infinity -> False
                    Dist h -> h >= 100
                )
            $
             map (\c -> (c, timeSaver m m' c)) (M.keys m')
    )
        $ (`findShortestDistance` s)
        $ Graph
        $ foldl (\m' k -> case m !? k of Just N -> constructEdges m k m'; _ -> m') M.empty ks
  where
    ks = M.keys m

----------- PART B -------------

partB :: Input -> OutputB
partB = error ""
