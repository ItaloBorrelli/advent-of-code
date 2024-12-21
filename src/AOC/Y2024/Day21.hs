module AOC.Y2024.Day21 (runDay) where

import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [(Int, Int, Int)]

type OutputA = Input

type OutputB = Void

type C = (Int, Int)

type A = Bool

----------- PARSER -------------

inputParser :: Parser Input
inputParser = ((\a b c -> (read [a], read [b], read [c])) <$> digit <*> digit <*> digit <* char 'A') `sepBy` newline

----------- PART A&B -----------

numpad :: Map Int C
numpad =
    fromList
        [ (7, (0, 0))
        , (8, (1, 0))
        , (9, (2, 0))
        , (4, (0, 1))
        , (5, (1, 1))
        , (6, (2, 1))
        , (1, (0, 2))
        , (2, (1, 2))
        , (3, (2, 2))
        , (0, (1, 3))
        , (-1, (2, 3))
        ]

----------- PART A -------------

partA :: Input -> OutputA
partA x = x

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
