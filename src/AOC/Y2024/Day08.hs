module AOC.Y2024.Day08 (runDay) where

import Data.Map.Strict (Map, empty, insertWith, keys, (!))
import Data.Tuple.Extra (both)
import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
  ( anyChar,
    char,
    eof,
    many,
    newline,
    sepBy,
    (<|>), satisfy,
  )
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type N = Char

data Z = N Char | E deriving (Show)

-- Coordinate
type C = (Int, Int)

type M = Map N [C]

type Input = [[Z]]

type OutputA = Int

type OutputB = Void

----------- PARSER -------------

parseLine :: Parser [Z]
parseLine = many (E <$ char '.' <|> N <$> satisfy (/= '\n'))

inputParser :: Parser Input
inputParser = parseLine `sepBy` newline <* eof

----------- PART A&B -----------

mapRow :: M -> C -> [Z] -> M
mapRow m _ [] = m
mapRow m (x, y) (E : ps) = mapRow m (x, y + 1) ps
mapRow m (x, y) (N p : ps) = insertWith (++) p [(x, y)] (mapRow m (x, y + 1) ps)

mapInput :: M -> C -> [[Z]] -> M
mapInput m _ [] = m
mapInput m (x, y) (r : rs) = mapRow (mapInput m (x + 1, y) rs) (x, y) r

inBounds :: (Int, Int) -> C -> Bool
inBounds (w, h) (x, y) = x >= 0 && y >= 0 && x < w && y < h

numberOfAntinodes :: (Int, Int) -> C -> C -> Int
numberOfAntinodes wh (xa, ya) (xb, yb) =
  let (s, t) = (((xb - xa) + xb, (yb - ya) + yb), ((xa - xb) + xa, (ya - yb) + ya))
   in uncurry (+) $ both (fromEnum . inBounds wh) (s, t)

checkList :: (Int, Int) -> [C] -> Int
checkList _ [] = 0
checkList wh (x : xs) = checkList wh xs + sum (map (numberOfAntinodes wh x) xs)

----------- PART A -------------

partA :: Input -> OutputA
partA input =
  let m = mapInput empty (0, 0) input
      wh = (length input, length $ head input)
   in sum $ map (\k -> checkList wh (m ! k)) (keys m)

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
