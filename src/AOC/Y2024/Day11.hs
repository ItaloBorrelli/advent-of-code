module AOC.Y2024.Day11 (runDay) where

import Data.Bifunctor (second)
import Data.Map.Lazy (Map, empty, insert, (!?))
import Data.Tuple.Extra (both)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, eof, many, sepBy)
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [Int]

type OutputA = Int

type OutputB = Int

type ValueMap = Map (Int, Int) Int

----------- PARSER -------------

inputParser :: Parser Input
inputParser = (read <$> many digit) `sepBy` char ' ' <* eof

----------- PART A&B -----------

blink1 :: Int -> [Int]
blink1 0 = [1]
blink1 s
  | even (length (show s)) =
      let (firstHalf, secondHalf) = both read $ splitAt (length (show s) `div` 2) (show s)
       in [firstHalf, secondHalf]
  | otherwise = [s * 2024]

blinkN :: Int -> ValueMap -> Int -> (ValueMap, Int)
blinkN n m s = case m !? (s, n) of
  Nothing
    | n == 1 -> (\c -> (insert (s, 1) c m, c)) $ length $ blink1 s
    | otherwise -> (\(m'', c'') -> (insert (s, n) c'' m'', c'')) $ foldl (\(m', c') s' -> second (c' +) $ blinkN (n - 1) m' s') (m, 0) (blink1 s)
  Just c -> (m, c)

----------- PART A -------------

partA :: Input -> OutputA
partA ss = snd $ foldl (\(m, c) s -> let (m', c') = blinkN 25 m s in (m', c + c')) (empty, 0) ss

----------- PART B -------------

partB :: Input -> OutputB
partB ss = snd $ foldl (\(m, c) s -> let (m', c') = blinkN 75 m s in (m', c + c')) (empty, 0) ss
