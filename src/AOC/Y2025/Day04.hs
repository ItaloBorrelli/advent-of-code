module AOC.Y2025.Day04 (runDay) where

import Data.Map (Map, insert, keys, (!), (!?))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text
import Util.Util (mapFromNestedLists)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

data S = E | T deriving (Show, Eq)

type C = (Int, Int)

type M = Map C S

type Input = M

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseLine :: Parser [S]
parseLine = many1 ((E <$ char '.') <|> (T <$ char '@'))

inputParser :: Parser Input
inputParser = mapFromNestedLists <$> parseLine `sepEndBy` newline

----------- PART A&B -----------

----------- PART A -------------

as :: [C]
as = [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)]

add :: C -> C -> C
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

adj :: M -> C -> [S]
adj m c = mapMaybe (\a -> m !? add a c) as

accessible :: M -> C -> Bool
accessible m c = case m ! c of
    E -> False
    T -> 4 > length (filter (== T) (adj m c))

partA :: Input -> OutputA
partA input = sum $ map (fromEnum . accessible input) (keys input)

----------- PART B -------------

removeRolls :: M -> [C] -> M
removeRolls = foldl (\m k -> insert k E m)

partB :: Input -> OutputB
partB input = go input
  where
    findRemovable m = filter (accessible m) (keys m)
    go m = if null q then 0 else length q + go (removeRolls m q)
      where
        q = findRemovable m
