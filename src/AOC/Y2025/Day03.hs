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

----------- PART A -------------

maxOutA :: (B, K) -> K -> (B, K)
maxOutA y [] = y
maxOutA y [_] = y
maxOutA (m, _) (x : xs)
    | x > m = if newBest > x then best else (x, xs)
    | otherwise = best
  where
    best = maxOutA (x, xs) xs
    (newBest, _) = best

ff :: K -> (B, K)
ff xs = maxOutA (0, xs) xs

partA :: Input -> OutputA
partA = sum . map (fst . (\xs -> foldl ffs (0, xs) [2, 1]))

----------- PART B -------------

ffs :: (B, K) -> Int -> (B, K)
ffs (total, xs) n = (\(best, remaining) -> (total + best * 10 ^ (n - 1), remaining)) $ maxOutB n (0, xs) xs

maxOutB :: Int -> (B, K) -> K -> (B, K)
maxOutB _ y [] = y
maxOutB n (m, ms) (x : xs)
    | length xs < n && x > m = (x, xs)
    | length xs < n = (m, ms)
    | x > m = if newBest > x then best else (x, xs)
    | otherwise = best
  where
    best = maxOutB n (x, xs) xs
    (newBest, _) = best

partB :: Input -> OutputB
partB = sum . map (fst . (\xs -> foldl ffs (0, xs) [12, 11 .. 1]))
