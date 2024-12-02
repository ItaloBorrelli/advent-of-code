module AOC.Y2024.Day02 (runDay) where

import           Control.Arrow    ((&&&))
import           Data.Function    (on)
import           Data.Maybe       (fromMaybe)
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

type OutputB = Int

------------ UTIL --------------
monotonic :: Int -> Int -> Bool
monotonic = (==) `on` (>0)

withinLimits :: Int -> Bool
withinLimits = uncurry (&&) . ((>=1) &&& (<=3)) . abs

isSafe :: Maybe Int -> [Int] -> Bool
isSafe firstInc (x1:x2:xs) =
    (\d -> withinLimits d && monotonic d (fromMaybe d firstInc) && isSafe (Just d) (x2:xs)) (x2 - x1)
isSafe _ _ = True

isSafeAfterRemoval :: Maybe Int -> Bool -> [Int] -> Bool
isSafeAfterRemoval firstInc hasError (x1:x2:xs) =
    (\d -> (withinLimits d && monotonic d (fromMaybe d firstInc) && isSafeAfterRemoval (Just d) False (x2:xs))
    ||
    (not hasError && isSafeAfterRemoval firstInc True (x1:xs))) (x2 - x1)
isSafeAfterRemoval _ _ _ = True

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter id . map (isSafe Nothing)

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter id . map (isSafeAfterRemoval Nothing False)
