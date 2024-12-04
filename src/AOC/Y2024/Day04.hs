module AOC.Y2024.Day04 (runDay) where

import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text
import Data.List (transpose)

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

------------ UTIL --------------

checkRow :: [XMAS] -> Int
checkRow (X:M:A:S:rest) = 1 + checkRow (S:rest)
checkRow (S:A:M:X:rest) = 1 + checkRow (X:rest)
checkRow (_:rest) = checkRow rest
checkRow _ = 0

checkRow2 :: [Maybe XMAS] -> Int
checkRow2 (Just X:Just M:Just A:Just S:rest) = 1 + checkRow2 (Just S:rest)
checkRow2 (Just S:Just A:Just M:Just X:rest) = 1 + checkRow2 (Just X:rest)
checkRow2 (_:rest) = checkRow2 rest
checkRow2 _ = 0

shiftRowForwardByN :: [Maybe XMAS] -> Int -> [Maybe XMAS]
shiftRowForwardByN [] _ = []
shiftRowForwardByN xs 0 = xs
shiftRowForwardByN (_:xs) n = shiftRowForwardByN (xs ++ [Nothing]) (n - 1)

------------ PART A ------------
partA :: Input -> OutputA
partA x =
    let (m, nums) = (length x, [0..(m-1)])
    in sum [sum (map checkRow x)
    , sum (map checkRow (transpose x))
    , sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (map Just) x) nums)))
    , sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (reverse . map Just) x) nums)))
    , sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (map Just) x) (reverse nums))))
    , sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (reverse . map Just) x) (reverse nums))))]

------------ PART B ------------
partB :: Input -> OutputB
partB _ = 1
