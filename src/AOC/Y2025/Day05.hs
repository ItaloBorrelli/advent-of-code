module AOC.Y2025.Day05 (runDay) where

import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text
import Util.Util (mapFromNestedLists)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type R = Int

type Input = ([C], [R])

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseC :: Parser C
parseC = ((,) . read <$> many1 digit) <*> (char '-' *> (read <$> many1 digit))

parseR :: Parser [R]
parseR = (read <$> many1 digit) `sepEndBy` newline

inputParser :: Parser Input
inputParser = (,) <$> parseC `endBy` newline <* newline <*> parseR

----------- PART A&B -----------

----------- PART A -------------

isFresh :: [C] -> R -> Bool
isFresh [] r = False
isFresh ((x1, x2) : xs) r = if r >= x1 && r <= x2 then True else isFresh xs r

partA :: Input -> OutputA
partA (cs, rs) = sum $ map (fromEnum . (isFresh cs)) rs

----------- PART B ------------
sort :: C -> C -> Ordering
sort (x, _) (y, _) = compare x y

combine :: [C] -> [C]
combine [] = []
combine [x] = [x]
combine (q@(q1, q2) : r@(r1, r2) : xs)
    | q2 < r1 = q : combine (r : xs)
    | otherwise = combine ((q1, max q2 r2) : xs)

sizea :: C -> Int
sizea (x, y) = y - x + 1

partB :: Input -> OutputB
partB (cs, _) = sum $ map sizea $ combine (sortBy sort cs)
