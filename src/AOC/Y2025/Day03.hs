module AOC.Y2025.Day03 (runDay) where

import Data.Bifunctor (Bifunctor (bimap, first), second)
import Data.Char (digitToInt)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type K = [B]

type B = Int

type Input = [K]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

inputParser :: Parser Input
inputParser = many1 (digitToInt <$> digit) `sepEndBy` newline

----------- PART A&B -----------

maxOut :: Int -> (B, K) -> K -> (B, K)
maxOut _ y [] = y
maxOut n (m, ms) (x : xs)
    | length xs < n && x > m = (x, xs)
    | length xs < n = (m, ms)
    | x > m = if newBest > x then best else (x, xs)
    | otherwise = best
  where
    best = maxOut n (x, xs) xs
    (newBest, _) = best

ffs :: (B, K) -> Int -> (B, K)
ffs (total, xs) n = (\(best, remaining) -> (total + best * 10 ^ (n - 1), remaining)) $ maxOut n (0, xs) xs

----------- PART A -------------

partA :: Input -> OutputA
partA = sum . map (fst . (\xs -> foldl ffs (0, xs) [2, 1]))

----------- PART B -------------

partB :: Input -> OutputB
partB = sum . map (fst . (\xs -> foldl ffs (0, xs) [12, 11 .. 1]))
