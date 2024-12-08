module AOC.Y2024.Day08 (runDay) where

import Data.Map.Strict (Map, elems, empty, insertWith)
import Data.Set (Set, fromList, union, unions)
import Data.Set qualified as S
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
  ( char,
    eof,
    many,
    newline,
    satisfy,
    sepBy,
    (<|>),
  )
import Text.Parsec.Text (Parser)
import Util.Util (tupleUp)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

-- Antenna
type A = Char

-- Value
data V = N Char | E deriving (Show)

-- Coordinate
type C = (Int, Int)

-- Map of Antennas to Coordinates
type M = Map A [C]

type Input = (C, M)

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseLine :: Parser [V]
parseLine = many (E <$ char '.' <|> N <$> satisfy (/= '\n'))

inputParser :: Parser Input
inputParser = (\i -> ((length i, length $ head i), mapInput empty (0, 0) i)) <$> (parseLine `sepBy` newline <* eof)

----------- PART A&B -----------

mapRow :: M -> C -> [V] -> M
mapRow m _ [] = m
mapRow m (x, y) (E : ps) = mapRow m (x, y + 1) ps
mapRow m (x, y) (N p : ps) = insertWith (++) p [(x, y)] (mapRow m (x, y + 1) ps)

mapInput :: M -> C -> [[V]] -> M
mapInput m _ [] = m
mapInput m (x, y) (r : rs) = mapRow (mapInput m (x + 1, y) rs) (x, y) r

inBounds :: (Int, Int) -> C -> Bool
inBounds (w, h) (x, y) = all (>= 0) [x, y] && x < w && y < h

numberOfAntinodes :: (Int, Int) -> C -> C -> Set C
numberOfAntinodes wh (xa, ya) (xb, yb) =
  let (s, t) = (((xb - xa) + xb, (yb - ya) + yb), ((xa - xb) + xa, (ya - yb) + ya))
   in fromList $ map snd $ filter fst $ map (\r -> (inBounds wh r, r)) [s, t]

checkList :: (Int, Int) -> [C] -> Set C
checkList _ [] = S.empty
checkList wh (x : xs) = checkList wh xs `union` unions (map (numberOfAntinodes wh x) xs)

----------- PART A -------------

partA :: Input -> OutputA
partA (wh, m) = length $ foldl (\s v -> s `union` checkList wh v) (S.empty :: Set C) (elems m)

----------- PART B -------------

fullCheck :: (Int, Int) -> (Int, Int) -> C -> Set C -> Set C
fullCheck wh diffs c s =
  let c' = tupleUp (+) diffs c
      bounded = inBounds wh c'
   in if bounded then S.insert c' (fullCheck wh diffs c' s) else s

numberOfAntinodes' :: (Int, Int) -> C -> C -> Set C
numberOfAntinodes' wh (xa, ya) (xb, yb) = fullCheck wh (xb - xa, yb - ya) (xb, yb) S.empty `union` fullCheck wh (xa - xb, ya - yb) (xa, ya) S.empty

checkList' :: (Int, Int) -> [C] -> Set C
checkList' _ [] = S.empty
checkList' wh (x : xs) = checkList' wh xs `union` unions (map (numberOfAntinodes' wh x) xs)

partB :: Input -> OutputB
partB (wh, m) = length $ foldl (\s v -> S.fromList v `union` s `union` checkList' wh v) (S.empty :: Set C) (elems m)
