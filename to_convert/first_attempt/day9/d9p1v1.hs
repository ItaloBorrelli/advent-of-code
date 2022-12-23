moveH :: (Int, Int) -> Char -> (Int, Int)
moveH (x,y) dir
    | dir == 'U' = (x,y+1)
    | dir == 'D' = (x,y-1)
    | dir == 'R' = (x+1,y)
    | dir == 'L' = (x-1,y)
    | otherwise = (x,y)

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (xH, yH) (xT, yT)
    | abs xDiff <= 1 && abs yDiff <= 1 = (xT, yT)
    | otherwise = (
        if xH == xT then xT else xT + (xDiff `div` abs xDiff),
        if yH == yT then yT else yT + (yDiff `div` abs yDiff)
        )
    where (xDiff, yDiff) = (xH-xT, yH-yT)

adjustMap :: ((Int, Int), (Int, Int), [[Bool]]) -> ((Int, Int), (Int, Int), [[Bool]])
adjustMap ((xH, yH), (xT, yT), visitMap) =
    let ((nxH, nxT, adjustMapX), (nyH, nyT, adjustMapY)) = (
            if xT < 0 then (xH+1, xT+1, map (False:) visitMap)
            else (xH, xT, visitMap),
            if yT < 0 then (yH+1, yT+1, replicate (length $ head adjustMapX) False:adjustMapX)
            else (yH, yT, adjustMapX)
            )
    in ((nxH, nyH), (nxT, nyT), adjustMapY)

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

adjustVisit :: ((Int, Int), [[Bool]], Int) -> ([[Bool]], Int)
adjustVisit ((x, y), m, visits)
    | visited = (m, visits)
    | otherwise = (setAt m row (setAt (m !! row) x True), visits+1)
    where (row, visited) = ((length m - y) - 1, (m !! row !! x))

move :: ((Int, Int), (Int, Int), [[Bool]], Int) -> Char -> ((Int, Int), (Int, Int), [[Bool]], Int)
move (h, t, visitMap, numVisits) dir =
    let (newHead, newTail, (trueHead, trueTail, adjustedVisitMap), (trueVisitMap, trueVisits)) = (
            moveH h dir,
            follow newHead t,
            adjustMap (newHead, newTail, visitMap),
            adjustVisit (trueTail, adjustedVisitMap, numVisits)
            )
    in (trueHead, trueTail, trueVisitMap, trueVisits)

move' :: ((Int, Int), (Int, Int), [[Bool]], Int) -> Char -> ((Int, Int), (Int, Int), [[Bool]], Int)
move' (h, t, visitMap, numVisits) dir =
    (\newHead ->
        (\newTail ->
            (\(trueHead, trueTail, adjustedVisitMap) ->
                (\(trueVisitMap, trueVisits) ->
                    (trueHead, trueTail, trueVisitMap, trueVisits))
                $ adjustVisit (trueTail, adjustedVisitMap, numVisits))
            $ adjustMap (newHead, newTail, visitMap))
        $ follow newHead t)
    $ moveH h dir

readInt :: String -> Int
readInt = read

parse :: [Char] -> [Char]
parse (dir:' ':num) = replicate (readInt num) dir

main :: IO ()
main = do
    contents <- readFile "./testinput"
    let lineList = lines contents
    let (_, _, visits, _) = foldl move' ((0, 0), (0, 0), [[True]], 1) $ foldr (++) [] $ map parse lineList
    print visits