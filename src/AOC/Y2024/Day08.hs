module AOC.Y2024.Day08 (runDay) where

import Data.Aviary.Birds (bluebird)
import Data.Map.Strict (Map, elems, empty, insertWith)
import Data.Set (Set, union)
import Data.Set qualified as S
import Data.Tuple.Extra (both)
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
import Util.Util (allFoldl, tupleUp, twiceAsNice)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

-- Antenna
type A = Char

-- Value
data V = N Char | E deriving (Show)

-- Coordinate
type C = (Int, Int)

-- Bounds
type B = (Int, Int)

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

checkWith :: (B -> (Int, Int) -> C -> Set C -> Set C) -> B -> C -> C -> (Set C -> Set C)
checkWith f wh (xa, ya) (xb, yb) = f wh (xa - xb, ya - yb) (xa, ya)

inBounds :: B -> C -> Bool
inBounds wh xy = twiceAsNice (&&) (both (>= 0) xy) (tupleUp (<) xy wh)

----------- PART A -------------

check1 :: B -> (Int, Int) -> C -> Set C -> Set C
check1 wh diffs c s =
  let c' = tupleUp (+) diffs c
      bounded = inBounds wh c'
   in if bounded then S.insert c' s else s

numberOfAntinodes :: B -> C -> Set C -> C -> Set C
numberOfAntinodes wh c1 s c2 = checkWith check1 wh c2 c1 $ checkWith check1 wh c1 c2 s

checkList :: B -> Set C -> [C] -> Set C
checkList = bluebird allFoldl numberOfAntinodes

partA :: Input -> OutputA
partA (wh, m) = length $ foldl (checkList wh) (S.empty :: Set C) (elems m)

----------- PART B -------------

fullCheck :: B -> (Int, Int) -> C -> Set C -> Set C
fullCheck wh diffs c s =
  let c' = tupleUp (+) diffs c
      bounded = inBounds wh c'
   in if bounded then S.insert c' (fullCheck wh diffs c' s) else s

numberOfAntinodes' :: B -> C -> Set C -> C -> Set C
numberOfAntinodes' wh c1 s c2 = checkWith fullCheck wh c2 c1 $ checkWith fullCheck wh c1 c2 s

checkList' :: B -> Set C -> [C] -> Set C
checkList' = bluebird allFoldl numberOfAntinodes'

partB :: Input -> OutputB
partB (wh, m) = length $ foldl (\s v -> S.fromList v `union` checkList' wh s v) (S.empty :: Set C) (elems m)
