module AOC.Y2024.Day19 (runDay) where

import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec ()
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = Void

type OutputA = Void

type OutputB = Void

----------- PARSER -------------

inputParser :: Parser Input
inputParser = fail "Parser not yet implemented!"

----------- PART A&B -----------

----------- PART A -------------

partA :: Input -> OutputA
partA = error "Not implemented yet!"

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
