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

isSafe :: Maybe Int -> Maybe Int -> [Int] -> Bool
isSafe Nothing Nothing (x:xs) = isSafe Nothing (Just x) xs
isSafe firstInc (Just prev) (x:xs) =
    (\d -> withinLimits d && monotonic d (fromMaybe d firstInc) && isSafe (Just d) (Just x) xs) (x - prev)
isSafe _ _ _ = True

isSafeAfterRemoval :: Maybe Int -> Maybe Int -> [Int] -> Bool -> Bool
isSafeAfterRemoval Nothing Nothing (x:xs) hasFailure = isSafeAfterRemoval Nothing (Just x) xs hasFailure || isSafeAfterRemoval Nothing (Just x) xs True
isSafeAfterRemoval firstInc (Just prev) (x:xs) hasFailure =
    (\d -> withinLimits d && monotonic d (fromMaybe d firstInc) && isSafeAfterRemoval (Just d) (Just x) xs hasFailure) (x - prev) || (not hasFailure && isSafeAfterRemoval Nothing (Just prev) xs True)
isSafeAfterRemoval _ _ _ _ = True

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter id . map (isSafe Nothing Nothing)

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter id . map (flip (isSafeAfterRemoval Nothing Nothing) False)
