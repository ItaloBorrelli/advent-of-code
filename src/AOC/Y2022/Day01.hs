module AOC.Y2022.Day01 (runDay) where

import           Control.Applicative    (optional)
import qualified Program.RunDay         as R (Day, runDay)
import           Text.Parsec            (digit)
import           Text.Parsec.Char       (newline)
import           Text.Parsec.Combinator (many1, sepEndBy)
import           Text.Parsec.Text       (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = optional (read <$> many1 digit) `sepEndBy` newline

------------ TYPES -------------
type Input = [Maybe Int]

type OutputA = Int

type OutputB = Int

------------ UTIL --------------

splitSum :: Input -> [Int]
splitSum = foldr f []
  where
    f Nothing acc     = 0 : acc
    f (Just x) (y:ys) = (x + y) : ys
    f (Just x) []     = [x]

insertIfLarger :: Ord a => a -> [a] -> [a]
insertIfLarger x [] = [x]
insertIfLarger x (y:ys)
    | x >= y    = x : y : ys
    | otherwise = y : insertIfLarger x ys

insertTopThree :: Ord a => [a] -> a -> [a]
insertTopThree top x = take 3 $ insertIfLarger x top

threeLargest :: Ord a => [a] -> [a]
threeLargest = foldl insertTopThree []

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . splitSum

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . threeLargest . splitSum
