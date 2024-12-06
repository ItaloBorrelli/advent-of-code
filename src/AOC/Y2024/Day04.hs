module AOC.Y2024.Day04 (runDay) where

import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, endBy, many, newline, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (both, safeTake, (!!?))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [[XMAS]]

type OutputA = Int

type OutputB = Int

data XMAS = X | M | A | S deriving (Eq, Show)

----------- PARSER -------------

parseLine :: Parser [XMAS]
parseLine = many ((X <$ char 'X') <|> (M <$ char 'M') <|> (A <$ char 'A') <|> (S <$ char 'S'))

inputParser :: Parser Input
inputParser = parseLine `endBy` newline

----------- PART A&B -----------

checkLeftToRight :: ([[XMAS]] -> Int) -> [[XMAS]] -> Int
checkLeftToRight _ [] = 0
checkLeftToRight evaluate grid
  | all null grid = 0
  | otherwise = evaluate grid + checkLeftToRight evaluate (map tail grid)

checkTopToBottom :: Int -> ([[XMAS]] -> Int) -> [[XMAS]] -> Int
checkTopToBottom _ _ [] = 0
checkTopToBottom windowSize evaluate xs = checkLeftToRight evaluate (safeTake windowSize xs) + checkTopToBottom windowSize evaluate (tail xs)

areValuesAtIndexes :: (Eq a) => [a] -> [[a]] -> [(Int, Int)] -> Bool
areValuesAtIndexes values grid idxs = and [(grid !!? i >>= (!!? j)) == Just current | ((i, j), current) <- zip idxs values]

----------- PART A -------------

zs4 :: [Int]
zs4 = replicate 4 0

zTo3 :: [Int]
zTo3 = [0 .. 3]

indexesOf4 :: [[(Int, Int)]]
indexesOf4 = uncurry zip <$> concatMap (\x -> [x, both reverse x]) [(zs4, zTo3), (zTo3, zs4), (zTo3, zTo3), (zTo3, reverse zTo3)]

check4x4 :: [[XMAS]] -> Int
check4x4 grid = sum $ map (fromEnum . areValuesAtIndexes [X, M, A, S] grid) indexesOf4

partA :: Input -> OutputA
partA = checkTopToBottom 4 check4x4

----------- PART B -------------

zTo2 :: [Int]
zTo2 = [0 .. 2]

twoTo0 :: [Int]
twoTo0 = [2, 1, 0]

doIndexesGiveMAS :: [[XMAS]] -> [(Int, Int)] -> Bool
doIndexesGiveMAS = areValuesAtIndexes [M, A, S]

checkMASInX :: [[XMAS]] -> Bool
checkMASInX grid =
  all
    (any (doIndexesGiveMAS grid . uncurry zip))
    [ [(zTo2, zTo2), (twoTo0, twoTo0)],
      [(zTo2, twoTo0), (twoTo0, zTo2)]
    ]

partB :: Input -> OutputB
partB = checkTopToBottom 3 (fromEnum . checkMASInX)
