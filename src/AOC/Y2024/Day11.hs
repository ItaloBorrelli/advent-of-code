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

update :: (Int, Int) -> (ValueMap, Int) -> (ValueMap, Int)
update k (m, c) = (insert k c m, c)

blinkN :: Int -> ValueMap -> Int -> (ValueMap, Int)
blinkN n m s = case m !? (s, n) of
  Nothing
    | n == 1 -> update (s, 1) (m, length $ blink1 s)
    | otherwise -> update (s, n) $ blinkAll (n - 1) m (blink1 s)
  Just c -> (m, c)

blinkAll :: Int -> ValueMap -> [Int] -> (ValueMap, Int)
blinkAll n m = foldl (\(m', c) -> second (c +) . blinkN n m') (m, 0)

----------- PART A -------------

partA :: Input -> OutputA
partA = snd . blinkAll 25 empty 

----------- PART B -------------

partB :: Input -> OutputB
partB = snd . blinkAll 75 empty 
