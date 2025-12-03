module AOC.Y2025.Day02 (runDay) where

import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------
type R = (Int, Int)

type Input = [R]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseRange :: Parser R
parseRange = (,) <$> (read <$> many1 digit <* char '-') <*> (read <$> many1 digit)

inputParser :: Parser Input
inputParser = parseRange `sepBy` char ','

----------- PART A&B -----------

----------- PART A -------------

findInvalid :: R -> Int
findInvalid (x, y)
    | x > y = 0
    | odd digits = findInvalid (10 ^ digits, y)
    | otherwise = val + findInvalid (x + 1, y)
  where
    digits = length (show x)
    pow = 10 ^ (digits `div` 2)
    frontHalf = x `div` pow
    backHalf = x - (frontHalf * pow)
    val = if frontHalf == backHalf then x else 0

partA :: Input -> OutputA
partA = sum . map findInvalid

----------- PART B -------------

factors :: (Int, Int) -> [Int]
factors (_, 0) = [1]
factors (_, 1) = [1]
factors (x, n)
    | x `mod` n == 0 = n : step
    | otherwise = step
  where
    step = factors (x, n - 1)

factorize :: Int -> [Int]
factorize x = factors (x, x - 1)

l :: Int -> Int
l = length . show

splitNumber :: Int -> Int -> Int -> [Int]
splitNumber 0 _ x = [x]
splitNumber s n x = front : splitNumber (s - n) n back
  where
    pow = 10 ^ s
    front = x `div` pow
    back = x - (front * pow)

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

findInvalid2 :: R -> Int
findInvalid2 (x, y)
    | x > y = 0
    | otherwise = (if anyRepeats then x else 0) + next
  where
    len = l x
    fs = factorize len
    anyRepeats = any (\f -> (\q -> allEqual q && length q > 1) $ splitNumber (len - f) f x) fs
    next = findInvalid2 (x + 1, y)

partB :: Input -> OutputB
partB = sum . map findInvalid2
