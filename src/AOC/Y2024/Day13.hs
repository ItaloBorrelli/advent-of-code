module AOC.Y2024.Day13 (runDay) where

import Data.Char (isDigit)
import Data.Tuple.Extra (both)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (digit, eof, many, newline, satisfy, sepBy, skipMany, skipMany1)
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type A = (Int, Int)

type B = (Int, Int)

type E = (A, B, C)

type Input = [E]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

num :: Parser Int
num = read <$> many digit

skipNonDigits :: Parser ()
skipNonDigits = skipMany (satisfy (not . isDigit))

parseDigitPair :: Parser (Int, Int)
parseDigitPair = (,) <$> (skipNonDigits *> num) <*> (skipNonDigits *> num)

parseE :: Parser E
parseE = (,,) <$> parseDigitPair <*> parseDigitPair <*> parseDigitPair

inputParser :: Parser [E]
inputParser = parseE `sepBy` skipMany1 newline <* eof

----------- PART A&B -----------

solve :: E -> Int
solve ((x1, y1), (x2, y2), (x, y)) =
    let
        d = (x1 * y2 - x2 * y1)
        ad = (y2 * x - x2 * y)
        bd = (x1 * y - y1 * x)
        m = (ad `mod` d) + (bd `mod` d)
     in
        if m /= 0 then 0 else (3 * ad `div` d) + bd `div` d

----------- PART A -------------

partA :: Input -> OutputA
partA = sum . map solve

----------- PART B -------------

alter :: E -> E
alter (a, b, c) = (a, b, both (10000000000000 +) c)

partB :: Input -> OutputB
partB = sum . map (solve . alter)
