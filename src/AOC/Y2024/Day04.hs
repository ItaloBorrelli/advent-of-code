module AOC.Y2024.Day04 (runDay) where

import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (both, (!!?))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES -------------

type Input = [[XMAS]]

type OutputA = Int

type OutputB = Int

data XMAS = X | M | A | S deriving (Eq, Show)

------------ PARSER ------------

parseLine :: Parser [XMAS]
parseLine = many ((X <$ char 'X') <|> (M <$ char 'M') <|> (A <$ char 'A') <|> (S <$ char 'S')) <* newline

inputParser :: Parser Input
inputParser = many parseLine <* eof

------------ UTIL --------------

safeTake :: Int -> [a] -> [a]
safeTake 0 _ = []
safeTake _ [] = []
safeTake n (x : xs) = x : safeTake (n - 1) xs

checkLeftToRight :: ([[XMAS]] -> Int) -> [[XMAS]] -> Int
checkLeftToRight _ [] = 0
checkLeftToRight evaluate grid
  | all null grid = 0
  | otherwise = evaluate grid + checkLeftToRight evaluate (map tail grid)

checkTopToBottom :: Int -> ([[XMAS]] -> Int) -> [[XMAS]] -> Int
checkTopToBottom _ _ [] = 0
checkTopToBottom windowSize evaluate xs = checkLeftToRight evaluate (safeTake windowSize xs) + checkTopToBottom windowSize evaluate (tail xs)

------------ PART A ------------

indexesOf4 :: [[(Int, Int)]]
indexesOf4 = map (uncurry zip) $ concatMap (\x -> [x, both reverse x]) [(replicate 4 0, [0 .. 3]), ([0 .. 3], replicate 4 0), ([0 .. 3], [0 .. 3]), ([0 .. 3], [3, 2 .. 0])]

check4x4 :: [[XMAS]] -> Int
check4x4 grid = sum $ map (fromEnum . (\idxs -> and [(grid !!? i >>= (!!? j)) == Just xmas | ((i, j), xmas) <- zip idxs [X, M, A, S]])) indexesOf4

partA :: Input -> OutputA
partA = checkTopToBottom 4 check4x4

------------ PART B ------------

doIndexesGiveMAS :: [[XMAS]] -> [(Int, Int)] -> Bool
doIndexesGiveMAS grid idxs = and [(grid !!? i >>= (!!? j)) == Just mas | ((i, j), mas) <- zip idxs [M, A, S]]

checkMASInX :: [[XMAS]] -> Bool
checkMASInX grid = all (any (doIndexesGiveMAS grid)) [[zip [0 .. 2] [0 .. 2], zip [2, 1, 0] [2, 1, 0]], [zip [0 .. 2] [2, 1, 0], zip [2, 1, 0] [0 .. 2]]]

partB :: Input -> OutputB
partB = checkTopToBottom 3 (fromEnum . checkMASInX)
