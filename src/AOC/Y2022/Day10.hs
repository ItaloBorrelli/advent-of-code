module AOC.Y2022.Day10 (runDay) where

import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = fail "Parser not yet implemented!"

------------ TYPES -------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ UTIL --------------

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
