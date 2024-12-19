module AOC.Y2024.Day15 (runDay) where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Map.Lazy (Map, insert, keys, (!))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists')

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type A = C

data X = O' | W' | N' | S deriving (Eq, Show)

data Y = O | W | N deriving (Eq, Show)

data Y' = OL | OR | WW | NN deriving (Eq, Show)

data D = U | R | D | L deriving (Eq, Show)

type Grid = [[X]]

type M = Map C Y

type M' = Map C Y'

type Input = (A, M, M', [D])

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseMapLine :: Parser [X]
parseMapLine = many (O' <$ char 'O' <|> W' <$ char '#' <|> N' <$ char '.' <|> S <$ char '@')

parseDirections :: Parser [D]
parseDirections = many (U <$ char '^' <|> R <$ char '>' <|> D <$ char 'v' <|> L <$ char '<')

start :: Grid -> C
start = head . concatMap (\(y, row) -> [(y, x) | (x, v) <- zip [0 ..] row, v == S]) . zip [0 ..]

toMap :: Grid -> M
toMap = mapFromNestedLists' . map (map (\case O' -> O; W' -> W; _ -> N))

rupdate :: [X] -> [Y']
rupdate [] = []
rupdate (v : vs) = case v of
    O' -> OL : OR : rupdate vs
    W' -> WW : WW : rupdate vs
    _ -> NN : NN : rupdate vs

mupdate :: Grid -> M'
mupdate = mapFromNestedLists' . map rupdate

inputParser :: Parser Input
inputParser = (\g d -> (start g, toMap g, mupdate g, concat d)) <$> parseMapLine `sepBy` newline <*> parseDirections `sepBy` newline <* eof

----------- PART A&B -----------

step :: D -> C -> C
step U = second (\x -> x - 1)
step R = first (1 +)
step D = second (1 +)
step L = first (\x -> x - 1)

move :: M -> D -> C -> (M, C)
move m d c =
    let
        c' = step d c
     in
        case m ! c' of
            O ->
                let
                    (m', o') = move m d c'
                 in
                    if o' == c' then (m, c) else (insert o' O m', c')
            W -> (m, c)
            N -> (m, c')

moveFish :: M -> D -> A -> (M, A)
moveFish m d a =
    let
        (m', a') = move m d a
     in
        if a' == a then (m, a) else (insert a' N m', a')

gps :: C -> Int
gps (x, y) = x + y * 100

boxCheck :: M -> Int
boxCheck m = foldl (\n k -> if m ! k == O then n + gps k else n) 0 (keys m)

----------- PART A -------------

partA :: Input -> OutputA
partA (a, m, _, ds) = boxCheck $ fst $ foldl (\(m', a') d -> moveFish m' d a') (m, a) ds

----------- PART B -------------

toMove :: M' -> D -> C -> Maybe [(Y', C, C)]
toMove m d from =
    let
        to = step d from
        obj = m ! to
     in
        case obj of
            WW -> Nothing
            NN -> Just [(m ! from, from, to)]
            _ ->
                if d == L || d == R
                    then case toMove m d to of
                        Nothing -> Nothing
                        Just cs -> Just ((m ! from, from, to) : cs)
                    else case if obj == OL then (toMove m d to, toMove m d (step R to)) else (toMove m d (step L to), toMove m d to) of
                        (Nothing, _) -> Nothing
                        (_, Nothing) -> Nothing
                        (Just lc', Just rc') -> Just ((m ! from, from, to) : (lc' ++ rc'))

moveFish' :: M' -> D -> A -> (M', A)
moveFish' m d a = case toMove m d a of
    Nothing -> (m, a)
    Just cs -> ((\m'' -> foldl (\m' (obj, _, c') -> insert c' obj m') m'' cs) (foldl (\m' (_, c, _) -> insert c NN m') m cs), step d a)

boxCheck' :: M' -> Int
boxCheck' m = foldl (\n k -> if m ! k == OL then n + gps k else n) 0 (keys m)

partB :: Input -> OutputB
partB ((x, y), _, m, ds) = boxCheck' $ fst $ foldl (\(m', a') d -> moveFish' m' d a') (m, (y * 2, x)) ds
