module AOC.Y2024.Day04 (runDay) where

import Data.Maybe (fromJust, isNothing)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util ((!!?), both)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
parseLine :: Parser [XMAS]
parseLine = many ((X <$ char 'X') <|> (M <$ char 'M') <|> (A <$ char 'A') <|> (S <$ char 'S')) <* newline

inputParser :: Parser Input
inputParser = many parseLine <* eof

------------ TYPES -------------
type Input = [[XMAS]]

type OutputA = Int

type OutputB = Int

data XMAS = X | M | A | S deriving (Eq, Show)

type XMASTrio = (XMAS, XMAS, XMAS)

type XMASNonet = (XMASTrio, XMASTrio, XMASTrio)
------------ UTIL --------------

indexesOf4 :: [[(Int, Int)]]
indexesOf4 = map (uncurry zip) $ concatMap (\x -> [x, both reverse x]) [(replicate 4 0, [0..3]), ([0..3], replicate 4 0), ([0..3], [0..3]), ([0..3], [3,2..0])]

check4x4 :: [[XMAS]] -> [Bool]
check4x4 grid = map (\idxs -> and [(grid !!? i >>= (!!? j)) == Just xmas | ((i, j), xmas) <- zip idxs [X,M,A,S] ]) indexesOf4

checkAcross1 :: [[XMAS]] -> Int
checkAcross1 [] = 0
checkAcross1 grid
    | all null grid = 0
    | otherwise = sum (map fromEnum $ check4x4 grid) + checkAcross1 (map tail grid)

safeTake :: Int -> [a] -> [a]
safeTake 0 _ = []
safeTake _ [] = []
safeTake n (x:xs) = x:safeTake (n-1) xs

checkDown1 :: [[XMAS]] -> Int
checkDown1 [] = 0
checkDown1 xs = checkAcross1 (safeTake 4 xs) + checkDown1 (tail xs)

indexesGiveMAS :: [(Int, Int)] -> [[XMAS]] -> Bool
indexesGiveMAS idxs grid = and [(grid !!? i >>= (!!? j)) == Just mas | ((i, j), mas) <- zip idxs [M,A,S]]

checkMASInX :: [[XMAS]] -> Bool
checkMASInX grid = (indexesGiveMAS (zip [0..2] [0..2]) grid || indexesGiveMAS (zip [2,1,0] [2,1,0]) grid) && (indexesGiveMAS (zip [0..2] [2,1,0]) grid || indexesGiveMAS (zip [2,1,0] [0..2]) grid)

checkAcross :: [[XMAS]] -> Int
checkAcross [] = 0
checkAcross grid
    | all null grid = 0
    | otherwise = fromEnum (checkMASInX grid) + checkAcross (map tail grid)

checkDown :: [[XMAS]] -> Int
checkDown [] = 0
checkDown xs = checkAcross (safeTake 3 xs) + checkDown (tail xs)

------------ PART A ------------
partA :: Input -> OutputA
partA = checkDown1

------------ PART B ------------
partB :: Input -> OutputB
partB = checkDown
