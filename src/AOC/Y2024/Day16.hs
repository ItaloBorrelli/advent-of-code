module AOC.Y2024.Day16 (runDay) where

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
import Util.Util (mapFromNestedLists')

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

data D = U | D | L | R deriving (Eq, Show, Ord)

type R = C

type E = C

data X = W' | N' | S | E deriving (Eq, Show)

data Y = W | N deriving (Eq, Show)

type Grid = [[X]]

type Grid' = [[Y]]

data Distance a = Dist a | Infinity
    deriving (Show, Eq)

type M = Map C Y

type Input = (R, E, M)

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
toMap = mapFromNestedLists' . map (map (\case W' -> W; _ -> N))

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

(!??) :: (Ord k) => Map k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (distanceMap !? key)

type V = (C, D)

type EdgeMap = Map V [(V, Int)]

newtype Graph = Graph
    {edges :: EdgeMap}
    deriving (Show)

data DijkstraState = DijkstraState
    { visitedSet :: Set V
    , distanceMap :: Map V (Distance Int)
    , nodeQueue :: MinPrioHeap (Distance Int) V
    }

findShortestDistance :: Graph -> V -> Map V (Distance Int)
findShortestDistance graph src = processQueue initialState
  where
    initialVisited = S.empty
    initialDistances = M.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> Map V (Distance Int)
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

opposite :: D -> D
opposite U = D
opposite D = U
opposite L = R
opposite R = L

makeEdges :: C -> D -> [D] -> [(V, (V, Int))]
makeEdges _ _ [] = []
makeEdges c dirFrom (x : xs)
    | dirFrom == x = (from, (to, 2001)) : rest
    | dirFrom == opposite x = (from, (to, 1)) : rest
    | otherwise = (from, (to, 1001)) : rest
  where
    from = (c, opposite dirFrom)
    to = (step x c, x)
    rest = makeEdges c dirFrom xs

constructEdges :: M -> C -> EdgeMap -> EdgeMap
constructEdges m c prev = foldl insertEdge prev moreEdges
  where
    adjs = map fromJust $ filter (/= Nothing) $ map (\d -> let c' = step d c in case m !? c' of Just N -> Just d; _ -> Nothing) [U, R, L, D]
    moreEdges = foldl1 (++) $ map (\v -> makeEdges c v adjs) adjs
    insertEdge acc (k, v) = M.insertWith (++) k [v] acc

----------- PART A -------------

partA :: Input -> Distance Int
partA (r, e, m) = (\m' -> minimum $ map (\d -> m' !?? (e, d)) [U, D, L, R]) $ (\g -> findShortestDistance g (r, U)) $ Graph $ foldl (\m' k -> case m !? k of Just N -> constructEdges m k m'; _ -> m') M.empty ks
  where
    ks = M.keys m

----------- PART B -------------

type Q = MinPrioHeap (Distance Int) V

data PathState = PathState {
  q :: Q,
  onPath :: Set V
}

findOnPath :: V -> V -> Map V (Distance Int) -> EdgeMap -> Set V
findOnPath src e distanceMap adjs = processQueue (PathState q s)
  where
    q = H.fromList [(Dist 0, src)]
    s = S.singleton src
    processQueue :: PathState -> Set V
    processQueue (PathState q0 s0) = case H.view q0 of
        Nothing -> S.empty
        Just (from@(dFrom, vFrom), q1) ->
          let nexts = filter (\(v, d) -> distanceMap ! v == addDist dFrom (Dist d)) $ adjs ! vFrom
          in foldNeighbor (PathState q1)

          --       pathTo = (\(c, d) -> c) $ (adjs ! vFrom)
          --    in
          --       if addDist prevDist (Dist pathTo) == newDist
          --           then
          --               if to == e
          --                   then x1
          --                   else case adjs !? src of
          --                       Nothing -> S.empty
          --                       Just unvisitedNeighbors -> processQueue curr $ foldl (\n tt -> foldNeighbor n tt) (PathState x1 q1) unvisitedNeighbors
          --           else S.empty
          -- where
          --   x1 = S.insert to x0
          --   foldNeighbor :: PathState -> V -> PathState
          --   foldNeighbor (PathState xs qs) v = PathState xs (H.insert (distanceMap ! v, v) qs)

partB :: Input -> OutputB
partB (r, e, m) = length $ findOnPath (r, U) (e, U) distances edges
  where
    ks = M.keys m
    edges = foldl (\m' k -> case m !? k of Just N -> constructEdges m k m'; _ -> m') M.empty ks
    distances = (\g -> findShortestDistance g (r, U)) $ Graph edges
