module AOC.Y2024.Day15 (runDay) where

import           Data.Void (Void)
import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec (char, (<|>), many, sepBy, newline, oneOf, eof)
import           Text.Parsec.Text (Parser)
import Data.Map.Lazy ( Map, insert, (!), keys,delete )
import Data.List (transpose)
import Util.Util (mapFromNestedLists)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type A = C

data X = O' | W' | N' | S deriving (Eq, Show)

data Y = O | W | N deriving (Eq, Show)

data D = U | R | D | L deriving (Eq, Show)

type Grid = [[X]]

type M = Map C Y

type Input = (A, M, [D])

type OutputA = Int

type OutputB = Void

----------- PARSER -------------

parseMapLine :: Parser [X]
parseMapLine = many (O' <$ char 'O' <|> W' <$ char '#' <|> N' <$ char '.' <|> S <$ char '@')

parseDirections :: Parser [D]
parseDirections = many (U <$ char '^' <|> R <$ char '>' <|> D <$ char 'v' <|> L <$ char '<')

start :: Grid -> C
start = head . concatMap (\(y, row) -> [(x, y) | (x, v) <- zip [0 ..] row, v == S]) . zip [0 ..]

toMap :: Grid -> M
toMap = mapFromNestedLists . transpose . map (map (\case O' -> O; W' -> W; _ -> N))

inputParser :: Parser Input
inputParser = (\g d -> (start g, toMap g, concat d)) <$> parseMapLine `sepBy` newline <*> parseDirections `sepBy` newline <* eof

----------- PART A&B -----------

step :: D -> C -> C
step U = second (\x -> x - 1)
step R = first (1+)
step D = second (1+)
step L = first (\x -> x - 1)

move :: M -> D -> C -> (M, C)
move m d c =
    let c' = step d c
    in case m ! c' of
        O -> let (m', o') = move m d c'
             in if o' == c' then (m, c) else (insert o' O m', c')
        W -> (m, c)
        N -> (m, c')

moveFish :: M -> D -> A -> (M, A)
moveFish m d a =
    let (m', a') = move m d a
    in if a' == a then (m, a) else (insert a' N m', a')

gps :: C -> Int
gps (y, x) = y + x * 100

boxCheck :: M -> Int
boxCheck m = foldl (\n k -> if m ! k == O then n + gps k else n) 0 (keys m)

----------- PART A -------------

partA :: Input -> OutputA
partA (a, m, ds) = boxCheck $ fst $ foldl (\(m', a') d -> moveFish m' d a') (m, a) ds

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
