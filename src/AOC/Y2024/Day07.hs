module AOC.Y2024.Day07 (runDay) where

import Program.RunDay qualified as R
  ( Day,
    runDay,
  )
import Text.Parsec
  ( char,
    digit,
    eof,
    many1,
    newline,
    sepBy,
    space,
  )
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

-- Test
type T = Int

-- Values
type V = [Int]

-- Line
type L = (T, V)

-- Binary operator
type O = Int -> Int -> Int

type Input = [L]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseLine :: Parser L
parseLine = (,) <$> (read <$> many1 digit <* char ':' <* space) <*> ((read <$> many1 digit) `sepBy` char ' ')

inputParser :: Parser Input
inputParser = parseLine `sepBy` newline <* eof

----------- PART A&B -----------

operate :: T -> [O] -> V -> Int -> Bool
operate t _ [] c = t == c
operate t ops (x:xs) c = any (\op -> operate t ops xs (c `op` x)) ops

runLine :: [O] -> L -> Int
runLine ops (t,xs) = if operate t ops (tail xs) (head xs) then t else 0

----------- PART A -------------

partA :: Input -> OutputA
partA = sum . map (runLine [(+),(*)])

----------- PART B -------------

(+-+) :: Int -> Int -> Int
(+-+) a b = read (show a ++ show b)

partB :: Input -> OutputB
partB = sum . map (runLine [(+),(*),(+-+)])