import Data.Bifunctor (bimap, first, second)

type Coord = (Int, Int)
type MinMax = ((Int, Int), (Int, Int))
type Quadrant = [[Bool]]
type Graph = (Quadrant, Quadrant, Quadrant, Quadrant)

moveHead :: Coord -> Char -> Coord
moveHead (x,y) dir
    | dir == 'U' = (x, y+1)
    | dir == 'D' = (x, y-1)
    | dir == 'R' = (x+1, y)
    | dir == 'L' = (x-1, y)
    | otherwise = (x, y)

follow :: Coord -> Coord -> Coord
follow (xH, yH) (xT, yT)
    | abs xDiff <= 1 && abs yDiff <= 1 = (xT, yT)
    | otherwise = (
        if xH == xT then xT else xT + (xDiff `div` abs xDiff),
        if yH == yT then yT else yT + (yDiff `div` abs yDiff)
        )
    where (xDiff, yDiff) = (xH-xT, yH-yT)

readInt :: String -> Int
readInt = read

parseInput :: [Char] -> [Char]
parseInput (dir:' ':num) = replicate (readInt num) dir

updateMinMax :: (MinMax, Coord) -> Char -> (MinMax, Coord)
updateMinMax currState i
    | newX < minX = (first $ first $ first (\_ -> newX)) newState
    | newX > maxX = (first $ first $ second (\_ -> newX)) newState
    | newY < minY = (first $ second $ first (\_ -> newY)) newState
    | newY > maxY = (first $ second $ second (\_ -> newY)) newState
    | otherwise = newState
    where ((((minX, maxX), (minY, maxY)), (x, y)), (newX, newY), newState) =
            (currState, moveHead (x, y) i, (((minX, maxX), (minY, maxY)), (newX, newY)))

getXYBoundary :: [Char] -> MinMax
getXYBoundary instL = fst $ foldl updateMinMax (((0, 0), (0, 0)), (0, 0)) instL

makeGraph :: [Char] -> Graph
makeGraph instL =
    let ((minX, maxX), (minY, maxY)) = getXYBoundary instL
    in (replicate (maxY+10) $ replicate (maxX+10) False,
    replicate (maxY+10) $ replicate ((abs minX)+10) False,
    replicate ((abs minY)+10) $ replicate ((abs minX)+10) False,
    replicate ((abs minY)+10) $ replicate (maxX+10) False)

whichQuadrant :: Coord -> Int
whichQuadrant (x, y)
    | x >= 0 && y >= 0 = 1
    | x < 0 && y >= 0 = 2
    | x < 0 = 3
    | otherwise = 4

getQuadrant :: Graph -> Int -> Quadrant
getQuadrant (q1, q2, q3, q4) qN
    | qN == 1 = q1
    | qN == 2 = q2
    | qN == 3 = q3
    | otherwise = q4

replaceQuadrant :: Graph -> Int -> Quadrant -> Graph
replaceQuadrant (q1, q2, q3, q4) qN newQ
    | qN == 1 = (newQ, q2, q3, q4)
    | qN == 2 = (q1, newQ, q3, q4)
    | qN == 3 = (q1, q2, newQ, q4)
    | otherwise = (q1, q2, q3, newQ)

setVal :: Coord -> Quadrant -> (Quadrant, Bool)
setVal (x, y) q =
    (\(row:rows) ->
        (\(col:cols) -> (take y q ++ (take x row ++ True:cols):rows, col))
        $ drop x row)
    $ drop y q

visit :: (Coord, Graph, Int) -> (Coord, Graph, Int)
visit (c, g, visits) =
    let ((x, y), qN, (newG, newV)) = (
            c,
            whichQuadrant c, 
            bimap (replaceQuadrant g qN) (\x -> visits + if x then 0 else 1)
                $ setVal (abs x, abs y) $ getQuadrant g qN)
    in (c, newG, newV)

move :: ([Coord], Graph, Int) -> Char -> ([Coord], Graph, Int)
move (ks, g, v) i =
    (\newHead ->
        (\newKs ->
            (\(newTail, newG, newV) ->
                (take (length newKs - 1) newKs ++ [newTail], newG, newV))
            $ visit (last newKs, g, v))
        $ scanl follow newHead $ tail ks)
    $ moveHead (head ks) i

main :: IO ()
main = do
    contents <- readFile "./input"
    let instructions = foldr (++) [] $ map parseInput $ lines contents
    let graph1 = makeGraph instructions
    let (_, _, visits1) = foldl move (replicate 2 (0, 0), graph1, 0) instructions
    print "part 1"
    print visits1
    let graph2 = makeGraph instructions
    let (_, _, visits2) = foldl move (replicate 10 (0, 0), graph2, 0) instructions
    print "part 2"
    print visits2