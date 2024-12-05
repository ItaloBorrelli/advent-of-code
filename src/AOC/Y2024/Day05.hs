module AOC.Y2024.Day05 (runDay) where

import Control.Arrow (Arrow (second))
import Data.Map.Strict (Map, fromListWith, (!?))
import Data.Tuple (swap)
import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, endBy, eof, many1, newline, sepBy)
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type OrderRule = (Int, Int)

type PageUpdate = [Int]

type OrderMap = Map Int [Int]

type Input = ([OrderRule], [PageUpdate])

type OutputA = Int

type OutputB = Void

----------- PARSER -------------

parseRule :: Parser OrderRule
parseRule = do
  ord1 <- read <$> many1 digit
  _ <- char '|'
  ord2 <- read <$> many1 digit
  return (ord1, ord2)

parsePages :: Parser [PageUpdate]
parsePages = (read <$> many1 digit) `sepBy` char ',' `sepBy` newline

inputParser :: Parser Input
inputParser = do
  rules <- parseRule `endBy` newline
  _ <- newline
  pages <- parsePages <* eof
  return (rules, pages)

----------- PART A&B -----------

combineKeys :: [OrderRule] -> OrderMap
combineKeys = fromListWith (++) . map (second (: []))

shouldXBeAfterY :: OrderMap -> Int -> Int -> Bool
shouldXBeAfterY m x y = case m !? y of
  Nothing -> False
  Just ls -> x `elem` ls

isRowGood :: OrderMap -> PageUpdate -> Bool
isRowGood _ [] = True
isRowGood m (x : xs) = isRowGood m xs && not (any (shouldXBeAfterY m x) xs)

getMiddle :: PageUpdate -> Int
getMiddle xs = xs !! (length xs `div` 2)

----------- PART A -------------

partA :: Input -> OutputA
partA (rules, pages) = sum $ map (getMiddle . fst) $ filter snd $ zip pages (map (isRowGood (combineKeys rules)) pages)

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
