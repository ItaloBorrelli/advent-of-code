module AOC.Y2024.Day11 (runDay) where

import Data.Bifunctor (second)
import Data.Map.Lazy (Map, empty, insert, singleton, (!?))
import Data.Tuple.Extra (both)
import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, eof, many, sepBy)
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [Int]

type OutputA = Int

type OutputB = Int

type ValueMap = Map Int (Map Int [Int])

----------- PARSER -------------

inputParser :: Parser Input
inputParser = (read <$> many digit) `sepBy` char ' ' <* eof

----------- PART A&B -----------

initTransforms :: ValueMap
initTransforms = insert 0 (insert 1 [1] empty) empty

blink1 :: Int -> [Int]
blink1 0 = [1]
blink1 s
  | even (length (show s)) =
      let (firstHalf, secondHalf) = both read $ splitAt (length (show s) `div` 2) (show s)
       in [firstHalf, secondHalf]
  | otherwise = [s * 2024]

blinkN :: Int -> ValueMap -> Int -> (ValueMap, [Int])
blinkN n m s = case m !? s of
  Nothing
    | n == 1 -> (\ss -> (insert s (singleton 1 ss) m, ss)) $ blink1 s
    | otherwise -> (\(m', ss') -> (insert s (singleton n ss') m', ss')) $ second (foldl (\ss' s' -> blink1 s' ++ ss') []) $ blinkN (n - 1) m s
  Just ns -> case ns !? n of
    Nothing -> (\(m', ss') -> (insert s (singleton n ss') m', ss')) $ second (foldl (\ss' s' -> blink1 s' ++ ss') []) $ blinkN (n - 1) m s
    Just ss -> (m, ss)

----------- PART A -------------

partA :: Input -> OutputA
partA ss = snd $ foldl (\(m, c) s -> let (m', ss') = blinkN 25 m s in (m', c + length ss')) (initTransforms, 0) ss

----------- PART B -------------

partB :: Input -> OutputB
partB ss = snd $ foldl (\(m, c) s -> let (m', ss') = blinkN 75 m s in (m', c + length ss')) (initTransforms, 0) ss
