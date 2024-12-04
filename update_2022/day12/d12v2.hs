import Data.Char (ord)
import Data.Maybe (fromJust, isNothing)
import Data.Bifunctor (first)

type Coord = (Int, Int)
type Vert = (Int, Coord)
type Node = (Coord, [Coord])

findIndex :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndex _ _ [] = Nothing
findIndex currIdx check (x:xs)
    | check x = Just currIdx
    | otherwise = findIndex (currIdx+1) check xs

findInGraph :: Eq a => Int -> a -> [[a]] -> Maybe Coord
findInGraph _ _ [] = Nothing
findInGraph idx val (g:gs)
    | isNothing q = findInGraph (idx+1) val gs
    | otherwise = Just (idx, fromJust q)
    where q = findIndex 0 (val ==) g

transformToVerts :: [Char] -> Coord -> [Maybe Vert]
transformToVerts [] _ = []
transformToVerts (l:ls) (x, y) =
    Just (if l == 'S' then ord 'a' else if l == 'E' then ord 'z' else ord l, (x, y)):transformToVerts ls (x, y+1)

canMove :: Int -> Int -> Bool
canMove from to = from >= to - 1

getVerticalNeighbour :: Vert -> Maybe [Maybe Vert] -> Maybe Coord
getVerticalNeighbour _ Nothing = Nothing
getVerticalNeighbour (h, _) (Just adjRow)
    | canMove h hAdj = Just cAdj
    | otherwise = Nothing
    where (hAdj, cAdj) = (fromJust $ adjRow !! 1)

getHorizontalNeighbour :: Vert -> Maybe Vert -> Maybe Coord
getHorizontalNeighbour _ Nothing = Nothing
getHorizontalNeighbour (h, _) (Just (hAdj, cAdj))
    | canMove h hAdj = Just cAdj
    | otherwise = Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

createNodesFromRow :: (Maybe [Maybe Vert], Maybe [Maybe Vert], Maybe [Maybe Vert]) -> [Node]
createNodesFromRow (prevRow, Just (prevV:(Just currV):nextV:vs), nextRow) =
    (currCoord, ln ++ rn ++ un ++ dn):
            if isNothing nextV then []
            else createNodesFromRow (
                    (if isNothing prevRow then Nothing else Just $ tail $ fromJust prevRow),
                    Just $ Just currV:nextV:vs,
                    (if isNothing nextRow then Nothing else Just $ tail $ fromJust nextRow)
            )
    where ((_, currCoord), ln, rn, un, dn) = (
            currV,
            maybeToList $ getHorizontalNeighbour currV prevV,
            maybeToList $ getHorizontalNeighbour currV nextV,
            maybeToList $ getVerticalNeighbour currV prevRow,
            maybeToList $ getVerticalNeighbour currV nextRow
            )

createEdgeList :: [Maybe [Maybe Vert]] -> [Node]
createEdgeList (a:b:[]) = []
createEdgeList (a:b:c:rest) = createNodesFromRow (a, b, c) ++ createEdgeList (b:c:rest)

findNode :: Coord -> [Node] -> Node
findNode c ns = head . snd $ span (\x -> fst x /= c) ns

extractNode :: Node -> [Node] -> (Node, [Node])
extractNode n ns = (n, a ++ tail b)
    where (a, b) = span (\x -> x /= n) ns

extractNodeFromCoord :: Coord -> [Node] -> Maybe (Node, [Node])
extractNodeFromCoord c ns
    | b == [] = Nothing
    | otherwise = Just (head b, a ++ tail b)
    where (a, b) = span (\x -> fst x /= c) ns

bfs :: [Node] -> [Node] -> Node -> Int -> Int
bfs adjList curr end dist
    | end `elem` curr = dist
    | otherwise = bfs newAdjList newCurr end (dist + 1)
    where (newCurr, newAdjList) = foldr (\coord -> \(nc, nadj) ->
            let t = extractNodeFromCoord coord $ nadj
            in if isNothing t then (nc, nadj) else first (:nc) $ fromJust t)
            ([] :: [Node], adjList)
            $ foldr (++) [] $ map (\x -> snd x) curr

getMinTravel :: [Node] -> (Coord, Coord) -> Int
getMinTravel adjList (start, end) = totalDist
    where ((startNode, rest), endNode, totalDist) =
            (extractNode (findNode start adjList) adjList, findNode end adjList, bfs rest [startNode] endNode 0)

main :: IO ()
main = do
    contents <- readFile "./input"
    let fileLines = lines contents
    let start = fromJust $ findInGraph 0 'S' fileLines
    let end = fromJust $ findInGraph 0 'E' fileLines
    let vertGraph = zipWith (\xCoord line -> Just $ (Nothing:)
            $ transformToVerts line (xCoord, 0) ++ [Nothing]) [0..]
            $ fileLines
    let adjList = createEdgeList $ [Nothing] ++ vertGraph ++ [Nothing]
    print $ getMinTravel adjList (start, end)