module AOC.Y2024.Day01 (runDay) where

import qualified Program.RunDay         as R (Day, runDay)
import           Text.Parsec            (digit, many1, space)
import           Text.Parsec.Char       (newline)
import           Text.Parsec.Combinator (sepEndBy)
import           Text.Parsec.Text       (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
splitLine :: Parser (Int, Int)
splitLine = do
  num1 <- many1 digit
  _ <- many1 space
  num2 <- many1 digit
  return (read num1, read num2)

inputParser :: Parser Input
inputParser = splitLine `sepEndBy` newline

------------ TYPES -------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ UTIL --------------
findAndRemoveMin :: Input -> (Input, (Int, Int))
findAndRemoveMin [] = error "Empty list"
findAndRemoveMin [(m1, m2)] = ([], (m1, m2))
findAndRemoveMin ((x1, x2):xs) =
    let (xs', (m1, m2)) = findAndRemoveMin xs
    in ((max m1 x1, max m2 x2):xs', (min m1 x1, min m2 x2))

countAndRemoveValue :: [Int] -> Int -> ([Int], Int)
countAndRemoveValue [] _ = ([], 0)
countAndRemoveValue (x:xs) v =
    let (xs', total) = countAndRemoveValue xs v
    in if x == v then (xs', total + 1) else (x:xs', total)

countAndRemoveLeft :: [Int] -> [Int] -> Int
countAndRemoveLeft [] _ = 0
countAndRemoveLeft _ [] = 0
countAndRemoveLeft (x:xs) ys =
    let (xs', countInLeft) = countAndRemoveValue xs x
        (ys', countInRight) = countAndRemoveValue ys x
        new_val = (x * (1 + countInLeft) * countInRight)
    in new_val + countAndRemoveLeft xs' ys'

------------ PART A ------------
partA :: Input -> OutputA
partA = go 0
    where
        go dist_sums xs =
            let (xs', (m1, m2)) = findAndRemoveMin xs
                new_sum = abs (m1 - m2) + dist_sums
            in if null xs' then new_sum else go new_sum xs'

------------ PART B ------------
partB :: Input -> OutputB
partB = uncurry countAndRemoveLeft . unzip
