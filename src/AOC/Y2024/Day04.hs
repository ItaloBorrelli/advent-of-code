module AOC.Y2024.Day04 (runDay) where

import Data.List (transpose)
import Data.Maybe (fromJust, isNothing)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, (<|>))
import Text.Parsec.Text (Parser)

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

checkRow :: [XMAS] -> Int
checkRow (X : M : A : S : rest) = 1 + checkRow (S : rest)
checkRow (S : A : M : X : rest) = 1 + checkRow (X : rest)
checkRow (_ : rest) = checkRow rest
checkRow _ = 0

checkRow2 :: [Maybe XMAS] -> Int
checkRow2 (Just X : Just M : Just A : Just S : rest) = 1 + checkRow2 (Just S : rest)
checkRow2 (Just S : Just A : Just M : Just X : rest) = 1 + checkRow2 (Just X : rest)
checkRow2 (_ : rest) = checkRow2 rest
checkRow2 _ = 0

shiftRowForwardByN :: [Maybe XMAS] -> Int -> [Maybe XMAS]
shiftRowForwardByN [] _ = []
shiftRowForwardByN xs 0 = xs
shiftRowForwardByN (_ : xs) n = shiftRowForwardByN (xs ++ [Nothing]) (n - 1)

first3 :: [XMAS] -> Maybe XMASTrio
first3 (x : y : z : _) = Just (x, y, z)
first3 _ = Nothing

firstNonet :: [[XMAS]] -> Maybe XMASNonet
firstNonet (x : y : z : _) = do
  a <- first3 x
  b <- first3 y
  c <- first3 z
  return (a, b, c)
firstNonet _ = Nothing

checkNonet :: XMASNonet -> Bool
checkNonet ((x1, _, x3), (_, A, _), (z1, _, z3)) = ((x1 == M && z3 == S) || (x1 == S && z3 == M)) && ((x3 == M && z1 == S) || (x3 == S && z1 == M))
checkNonet _ = False

moving3x3WindowAccum :: [[XMAS]] -> Int
moving3x3WindowAccum [[], [], []] = 0
moving3x3WindowAccum (x : xs) =
  let non = firstNonet (x : xs)
      good = if isNothing non then 0 else fromEnum (checkNonet (fromJust non))
   in good + moving3x3WindowAccum (map tail (x : xs))
moving3x3WindowAccum _ = 0

moving3x3WindowAccum2 :: [[XMAS]] -> Int
moving3x3WindowAccum2 (x : y : z : rest) = moving3x3WindowAccum [x, y, z] + moving3x3WindowAccum2 (y : z : rest)
moving3x3WindowAccum2 _ = 0

------------ PART A ------------
partA :: Input -> OutputA
partA x =
  let (m, nums) = (length x, [0 .. m])
   in sum
        [ sum (map checkRow x),
          sum (map checkRow (transpose x)),
          sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (map Just) x) nums))),
          sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (reverse . map Just) x) nums))),
          sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (map Just) x) (reverse nums)))),
          sum (map checkRow2 (transpose $ (map . uncurry) shiftRowForwardByN (zip (map (reverse . map Just) x) (reverse nums))))
        ]

------------ PART B ------------
partB :: Input -> OutputB
partB = moving3x3WindowAccum2
