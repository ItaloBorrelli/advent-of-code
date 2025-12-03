module AOC.Y2025.Day01 (runDay) where

import Data.Bifunctor
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Move = (Dir, Int)

type Input = [Move]

type P = Int

type T = (Int, P)

data Dir = L | R deriving (Show)

type OutputA = Int

type OutputB = Int

----------- PARSER -------------
readLine :: Parser (Dir, Int)
readLine = (,) <$> ((L <$ char 'L') <|> (R <$ char 'R')) <*> (read <$> many1 digit)

inputParser :: Parser Input
inputParser = readLine `sepEndBy` newline <* eof

----------- PART A&B -----------

f :: P -> P
f x = (100 - x) `mod` 100

----------- PART A -------------

moveA :: T -> Move -> T
moveA state (L, dist) = second f $ moveA (second f state) (R, dist)
moveA (x, pos) (R, dist) =
    let
        pos' = (pos + dist) `mod` 100
     in
        (if pos' == 0 then x + 1 else x, pos')

partA :: Input -> OutputA
partA = fst . foldl moveA (0, 50)

----------- PART B -------------

moveB :: T -> Move -> T
moveB state (L, dist) = second f $ moveB (second f state) (R, dist)
moveB (x, pos) (R, dist) =
    let
        pos' = (pos + dist)
     in
        (x + (pos' `div` 100), pos' `mod` 100)

partB :: Input -> OutputB
partB = fst . foldl moveB (0, 50)
