module AOC.Y2024.Day14 (runDay) where

import Data.Bifunctor (Bifunctor (first), second)
import Data.Map.Lazy (Map, empty, insert, insertWith, keys, member, (!))
import Data.Tuple.Extra (both)
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
    ( char
    , digit
    , eof
    , many1
    , newline
    , sepBy
    , string
    , (<|>)
    )
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type W = Int

type H = Int

type Dim = (W, H)

type C = (Int, Int)

type P = C

type V = (Int, Int)

type B = (P, V)

type Input = (Dim, [B])

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

num :: Parser Int
num = read <$> many1 (digit <|> char '-')

parseBot :: Parser B
parseBot = do
    _ <- string "p="
    a <- num
    _ <- string ","
    b <- num
    _ <- string " v="
    c <- num
    _ <- string ","
    d <- num
    return ((a, b), (c, d))

parseSize :: Parser Dim
parseSize = do
    a <- num
    _ <- char ','
    b <- num
    _ <- newline
    return (a, b)

inputParser :: Parser Input
inputParser = (,) <$> parseSize <*> parseBot `sepBy` newline <* eof

----------- PART A&B -----------

----------- PART A -------------

move100 :: Dim -> B -> P
move100 (w, h) ((x, y), (dx, dy)) = ((100 * dx + x) `mod` w, (100 * dy + y) `mod` h)

quad :: (Int, Int) -> P -> Int
quad (mx, my) (x, y)
    | x < mx && y < my = 0
    | x < mx && y > my = 2
    | x > mx && y < my = 1
    | x > mx && y > my = 3
    | otherwise = 4

categorize :: (Int, Int) -> [P] -> Int
categorize (mx, my) = (\m -> product $ map (m !) [0, 1, 2, 3]) . foldl (\q p -> insertWith (+) (quad (mx, my) p) 1 q) empty

partA :: Input -> OutputA
partA (dim, bs) = categorize (both (`div` 2) dim) $ map (move100 dim) bs

----------- PART B -------------

move1 :: Dim -> B -> B
move1 (w, h) ((x, y), (dx, dy)) = (((dx + x) `mod` w, (dy + y) `mod` h), (dx, dy))

createMap :: [B] -> Map C B
createMap = foldl (\m b -> insert (fst b) b m) empty

up, down, right, left :: C -> C
up = first (\x -> x - 1)
down = first (+ 1)
left = second (\x -> x - 1)
right = second (+ 1)

traversal :: Map C B -> [C] -> C -> [C]
traversal m v c =
    let
        cs = filter (\current -> member current m && current `notElem` v) $ map (\f -> f c) [up, down, right, left]
     in
        foldl (traversal m) (c : v) cs

checkBlockSize :: Int -> Int -> [C] -> Map C B -> Bool
checkBlockSize _ _ [] _ = False
checkBlockSize checks tol (v : vs) m =
    (checks < tol)
        && ( let
                block = traversal m [] v
              in
                ((length block >= tol) || checkBlockSize (checks + 1) tol vs m)
           )

visualizeGrid :: Dim -> [C] -> String
visualizeGrid (width, height) coords =
    unlines
        [ [if (x, y) `elem` coords then '#' else '.' | x <- [0 .. width - 1]]
        | y <- [0 .. height - 1]
        ]

progress :: Int -> Dim -> [B] -> (Bool, [B])
progress tol dim bs =
    let
        bs' = map (move1 dim) bs
        m = createMap bs'
     in
        (checkBlockSize 0 tol (keys m) m, bs')

partB :: Input -> OutputB
partB (dim, bs) =
    let
        tol = (length bs `div` 2) + 1
     in
        go tol dim bs 1
  where
    go tol d bs' s =
        let
            (tree, bs'') = progress tol d bs'
         in
            if tree
                then trace ("\n" ++ visualizeGrid dim (map fst bs'')) s
                else go tol d bs'' s + 1
