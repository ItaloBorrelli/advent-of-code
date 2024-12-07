module AOC.Y2022.Day01 (runDay) where

import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (digit, eof)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (many1, sepEndBy)
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [[Int]]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseChunk :: Parser [Int]
parseChunk = many1 ((read <$> many1 digit) <* newline)

-- Expects additional newline after last number
inputParser :: Parser Input
inputParser = parseChunk `sepEndBy` newline <* eof

----------- PART A -------------

partA :: Input -> OutputA
partA = maximum . map sum

----------- PART B -------------

insertIfLarger :: (Ord a) => [a] -> a -> [a]
insertIfLarger [] x = [x]
insertIfLarger (y : ys) x
  | x >= y = x : ys
  | otherwise = y : insertIfLarger ys x

insertTopThree :: (Ord a) => [a] -> a -> [a]
insertTopThree top x = take 3 $ insertIfLarger top x

maximum3 :: (Ord a) => [a] -> [a]
maximum3 = foldl insertTopThree []

partB :: Input -> OutputB
partB = sum . maximum3 . map sum
