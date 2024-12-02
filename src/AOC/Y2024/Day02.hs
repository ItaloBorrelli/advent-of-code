module AOC.Y2024.Day02 (runDay) where

import           Data.Void        (Void)
import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec      (char, digit, many1, newline, sepBy)
import           Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (read <$> many1 digit) `sepBy` char ' ' `sepBy` newline

------------ TYPES -------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Void

------------ UTIL --------------
isSafe :: Maybe Bool -> [Int] -> Bool
isSafe _ []               = True
isSafe _ [_]              = True
isSafe Nothing (x1:x2:xs) = abs(x2-x1) <= 3 && abs(x2-x1) >= 1 && isSafe (Just ((x2-x1) > 0)) (x2:xs)
isSafe (Just isInc) (x1:x2:xs) = (x2-x1 > 0) == isInc && abs(x2-x1) <= 3 && abs(x2-x1) >= 1 && isSafe (Just isInc) (x2:xs)

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter id . map (isSafe Nothing)
------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
