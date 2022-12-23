import Data.Char (ord)
import Data.Graph (Edge, graphFromEdges)

type Coord = (Int, Int)
type Vert = (Int, Coord)
type Node = (Coord, Coord, [Coord])

getEdgesForNode :: Vert -> [Vert] -> Node
getEdgesForNode x adjs = (snd x, snd x, map snd 
    $ filter (\adj -> fst adj <= 1 + fst x && fst adj /= -1) adjs)

getEdgesForRow :: ([Vert], [Vert], [Vert]) -> [Node]
getEdgesForRow (_, [], []) = []
getEdgesForRow (prevR, prevNode:currNode:nextNode:rest, nextR) = newNode:restOfNodes
    where (newNode, restOfNodes)
        | prevR == [] = (
            getEdgesForNode currNode [((head . tail) prevR), ((head . tail) nextR), prevNode, nextNode],
            getEdgesForRow (prevR, currNode:nextNode:rest, nextR)
            )

transformToVerts :: [Char] -> Coord -> [Vert]
transformToVerts [] c = [(-1, c)]
transformToVerts (l:ls) (x, y) = (if l == ' ' then -1 else ord l, (x, y)):transformToVerts ls (x, y+1)

main :: IO ()
main = do
    contents <- readFile "./testinput"
    let lineList = zipWith (\a -> \b -> transformToVerts a (b, -1)) (map (' ':) $ lines contents) [0..]
    print lineList
    let edgeList = getEdgesForRow ([], head lineList, (head . tail) lineList)
    print edgeList
    print 'a'