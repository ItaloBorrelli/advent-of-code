module AOC.Y2024.Day16 (runDay) where

import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as HM
import qualified Data.Heap as H
import Data.Set qualified as HS
import Data.Maybe (fromMaybe, fromJust)
import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists')
import Data.Set (Set)
import Data.Heap (MinPrioHeap)
import Data.Bifunctor (first, second)

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

type OutputA = Input

type OutputB = Void

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
    Infinity <= Dist x = False
    Dist x <= Infinity = True
    Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Eq k, Ord k) => Map k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (distanceMap !? key)

type M' = Map (C, D) [((C, D), Int)]
newtype Graph = Graph
   { edges :: M' } deriving (Show)

data DijkstraState = DijkstraState
  { visitedSet :: Set (C, D)
  , distanceMap :: Map (C, D) (Distance Int)
  , nodeQueue :: MinPrioHeap (Distance Int) (C, D)
  }


findShortestDistance :: Graph -> (C, D) -> (C, D) -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> Map (C, D) (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((minDist, node), q1) -> if node == dest then d0
        else if HS.member node v0 then processQueue (ds {nodeQueue = q1})
        else
          -- Update the visited set
          let v1 = HS.insert node v0
          -- Get all unvisited neighbors of our current node
              allNeighbors = fromMaybe [] (HM.lookup node (edges graph))
              unvisitedNeighbors = filter (\(n, _) -> not (HS.member n v1)) allNeighbors
          -- Fold each neighbor and recursively process the queue
          in  processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? current) (Dist cost)
      in  if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (HM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
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

makeEdges :: C -> D -> [D] -> [((C, D), ((C, D), Int))]
makeEdges _ _ [] = []
makeEdges c dirFrom (x:xs)
  | dirFrom == x = (from, (to, 2001)):rest
  | dirFrom == opposite x = (from, (to, 1)):rest
  | otherwise = (from, (to, 1001)):rest
  where from = (c, opposite dirFrom)
        to = (step x c, x)
        rest = makeEdges c dirFrom xs

constructEdges :: M -> C -> M' -> M'
constructEdges m c prev = foldl insertEdge prev moreEdges
  where 
    adjs = map fromJust $ filter (/= Nothing) $ map (\d -> let c' = step d c in case m !? c' of Just N -> Just d; _ -> Nothing) [U, R, L, D]
    moreEdges = foldl1 (++) $ map (\v -> makeEdges c v adjs) adjs
    insertEdge acc (k, v) = HM.insertWith (++) k [v] acc

----------- PART A -------------

partA :: Input -> Distance Int
partA (r, e, m) = (\g -> findShortestDistance g (r, U) (e, U)) $ Graph $ foldl (\m' k -> case m !? k of Just N -> constructEdges m k m'; _ -> m') HM.empty ks
  where ks = HM.keys m

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
