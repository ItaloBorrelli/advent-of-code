module AOC.Y2022.Day01 (runDay) where

import           Data.Attoparsec.Text
import           Data.Void
import qualified Program.RunDay       as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = fail "Parser not yet implemented!"

------------ TYPES -------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ CODE --------------

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
