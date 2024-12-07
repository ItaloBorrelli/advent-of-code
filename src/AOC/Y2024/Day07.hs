module AOC.Y2024.Day07 (runDay) where

import Program.RunDay qualified as R
    ( Day, runDay, Day, runDay, Day, runDay )
import Text.Parsec
    ( char,
      eof,
      newline,
      sepBy,
      digit,
      many1,
      space,
      char,
      digit,
      many1,
      char,
      eof,
      newline )
import Text.Parsec.Text ( Parser, Parser, Parser, Parser )

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type A = Int
type B = [Int]
type C = (A, B)

type Input = [C]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseLine :: Parser C
parseLine = do
    a <- read <$> many1 digit <* char ':'
    _ <- space
    b <- (read <$> many1 digit) `sepBy` char ' '
    return (a,b)

inputParser :: Parser Input
inputParser = parseLine `sepBy` newline <* eof

----------- PART A&B -----------


----------- PART A -------------

operate1 :: A -> Int -> B -> Bool
operate1 a c [] = a == c
operate1 a c (y:xs) = operate1 a (c * y) xs || operate1 a (c + y) xs

partA :: Input -> OutputA
partA = sum . map fst . filter snd . map (\(a, xs) -> (a, operate1 a (head xs) (tail xs)))

----------- PART B -------------

concatNums :: Int -> Int -> Int
concatNums a b = read (show a ++ show b)

operate2 :: A -> Int -> B -> Bool
operate2 a c [] = a == c
operate2 a c (y:xs) = operate2 a (c * y) xs || operate2 a (c + y) xs || operate2 a (c `concatNums` y) xs

partB :: Input -> OutputB
partB = sum . map fst . filter snd . map (\(a, xs) -> (a, operate2 a (head xs) (tail xs)))
