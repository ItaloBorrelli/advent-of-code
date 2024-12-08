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
import qualified Data.Set as S
import Data.Set (Set, fromList, unions, union)

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

numberOfAntinodes :: (Int, Int) -> C -> C -> Set C
numberOfAntinodes wh (xa, ya) (xb, yb) =
  let (s, t) = (((xb - xa) + xb, (yb - ya) + yb), ((xa - xb) + xa, (ya - yb) + ya))
   in fromList $ map snd $ filter fst $ map (\r -> (inBounds wh r, r)) [s, t]

checkList :: Set C -> (Int, Int) -> [C] -> Set C
checkList s _ [] = s
checkList s wh (x : xs) =
    let s' = unions (map (numberOfAntinodes wh x) xs)
    in checkList (s `union` s') wh xs

----------- PART A -------------

partA :: Input -> OutputA
partA input =
  let m = mapInput empty (0, 0) input
      wh = (length input, length $ head input)
   in length $ foldl (\s k -> checkList s wh (m ! k)) (S.empty :: Set C) (keys m)

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
