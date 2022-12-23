import Data.Array ((!))
import Data.Char (ord)
import Data.Maybe (fromJust, isNothing)
import Data.List (foldl', mapAccumL)
import Data.Set (fromList, insert, Set)
import Data.Bifunctor (first)
import Data.Graph (Graph, Vertex, graphFromEdges)

type Coord = (Int, Int)
data Node = Node { pos :: Coord, height :: Int, adj :: [Node] } deriving (Show)
addAdjs :: Node -> [Node] -> Node
addAdjs n as = Node (pos n) (height n) (as ++ adj n)

findInGraphMulti :: Eq a => a -> [[a]] -> Int -> [Coord]
findInGraphMulti _ [] _ = []
findInGraphMulti val (g:gs) idxX = findInGraphMulti val gs (idxX + 1) ++
        (map snd
        $ filter (\a -> fst a == val)
        $ zipWith (\i -> \idxY -> (i, (idxX, idxY))) g [0..])

transToNode :: [Char] -> Coord -> [Node]
transToNode [] _ = []
transToNode (l:ls) (x, y) =
    Node (x, y) (if l == 'S' then ord 'a' else if l == 'E' then ord 'z' else ord l) []:transToNode ls (x, y+1)

canMove :: Node -> Node -> Bool
canMove from to = height from >= height to - 1

ifHasMoveableAdj :: Bool -> Node -> Node -> [Node]
ifHasMoveableAdj True _ _ = []
ifHasMoveableAdj False x y
    | canMove x y = [y]
    | otherwise = []

updateAdjacencies :: ([Node], [Node], [Node]) -> Maybe Node -> [(Coord, Coord, [Coord])]
updateAdjacencies (_, [], _) _ = []
updateAdjacencies (prevRow, currRow, nextRow) prevInRow = (pos currNode, pos currNode, map pos adjList):(updateAdjacencies nextTuple $ Just currNode)
    where (currNode, adjList, nextTuple) = (
            head currRow,
            (ifHasMoveableAdj (isNothing prevInRow) currNode $ fromJust prevInRow)
            ++ (ifHasMoveableAdj (0 == (length $ tail currRow)) currNode $ currRow !! 1)
            ++ (ifHasMoveableAdj (0 == length prevRow) currNode $ head prevRow)
            ++ (ifHasMoveableAdj (0 == length nextRow) currNode $ head nextRow),
            (if 0 == length prevRow then [] else tail prevRow, tail currRow, if 0 == length nextRow then [] else tail nextRow)
            )

createEdges :: [[Node]] -> [(Coord, Coord, [Coord])]
createEdges (a:b:c:rest) = updateAdjacencies (a, b, c) Nothing
    ++ if (length c == 0) then [] else createEdges (b:c:rest)
createEdges _ = []

insertAll :: Ord a => [a] -> Set a -> Set a
insertAll xs s = foldl' (flip insert) s xs

bfs :: Graph -> Vertex -> (Int, Set Vertex, Set Vertex) -> (Int, Set Vertex, Set Vertex)
bfs g end (dist, xs, vs) = if end `elem` xs then (dist, xs, vs) else
    bfs g end
    $ foldr
    (\x -> \(d, ys, newVs) ->
        (\xAdj ->
            (d, insertAll xAdj ys, insertAll xAdj newVs))
    $ filter (\adj -> not $ adj `elem` vs) $ g ! x)
    (dist + 1, fromList [], vs) xs

main :: IO ()
main = do
    contents <- readFile "./input"
    let fileLines = lines contents
    let edges = createEdges $ []:zipWith (\line -> \x -> transToNode line (x, 0)) fileLines [0..] ++ [[]]
    let (x, _, z) = graphFromEdges edges
    let sCoord = findInGraphMulti 'S' fileLines 0 !! 0
    let sVertex = (fromJust . z) $ sCoord
    let eVertex = (fromJust . z) $ (findInGraphMulti 'E' fileLines 0) !! 0
    let aVertexs = map (fromJust . z) $ sCoord:findInGraphMulti 'a' fileLines 0
    let (dist1, _, _) = bfs x eVertex (0, fromList [sVertex], fromList [sVertex])
    let (dist2, _, _) = bfs x eVertex (0, fromList aVertexs, fromList aVertexs)
    print "part1"
    print dist1
    print "part2"
    print dist2