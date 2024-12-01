module AOC.Y2022.Day02 (runDay) where

import           Control.Monad    (liftM2)
import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec      (anyChar, char, newline, sepEndBy)
import           Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
charSpaceChar :: Parser (Char, Char)
charSpaceChar = liftM2 (,) anyChar (char ' ' >> anyChar)

inputParser :: Parser Input
inputParser = charSpaceChar `sepEndBy` newline

------------ TYPES -------------
type Input = [(Char, Char)]

type OutputA = Int

type OutputB = Int

data Result = Win | Draw | Lose
    deriving (Show, Eq)

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

type Match1 = (Shape, Shape)

type Match2 = (Shape, Result)

------------ UTIL --------------

pointsForResult :: Result -> Int
pointsForResult Win  = 6
pointsForResult Draw = 3
pointsForResult Lose = 0

pointsForShape :: Shape -> Int
pointsForShape Rock     = 1
pointsForShape Paper    = 2
pointsForShape Scissors = 3

result :: Match1 -> Result
result (x, y) | x == y   = Draw
result (Scissors, Rock)  = Win
result (Rock, Paper)     = Win
result (Paper, Scissors) = Win
result _                 = Lose

toShape :: Char -> Shape
toShape 'A' = Rock
toShape 'B' = Paper
toShape 'C' = Scissors
toShape _   = error "Invalid shape in input"

match1 :: (Char, Char) -> Match1
match1 (x, 'X') = (toShape x, Rock)
match1 (x, 'Y') = (toShape x, Paper)
match1 (x, 'Z') = (toShape x, Scissors)
match1 (_, _)   = error "Invalid shape in input"

match2 :: (Char, Char) -> Match2
match2 (x, 'X') = (toShape x, Lose)
match2 (x, 'Y') = (toShape x, Draw)
match2 (x, 'Z') = (toShape x, Win)
match2 (_, _)   = error "Invalid result in input"

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map ( (\(a, b) -> pointsForShape b + pointsForResult (result (a, b))) . match1)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map ( (\(a, b) -> pointsForResult b + pointsForShape (head [s | s <- [Rock, Paper, Scissors], result (a, s) == b])) . match2)
