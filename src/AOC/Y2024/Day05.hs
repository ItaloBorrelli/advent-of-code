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

-- must query be before current in the ordering rules given m mapping an int to all pages that it MUST be after
isPageBefore :: OrderMap -> Int -> Int -> Bool
isPageBefore m current query = case m !? current of
  Nothing -> False -- i.e. we don't know if there's any pages that explicitly come before this book so it's not violating current
  Just prevPages ->
    query `elem` prevPages -- if q is in prevPages then q is before current and therefore we fail
      || any (\p -> isPageBefore m p query) prevPages -- but if it wasn't, we need to check if any of the prevPages lead to q

areAnyPagesEarly :: OrderMap -> Int -> [Int] -> Bool
areAnyPagesEarly _ _ [] = False
areAnyPagesEarly m x ys = any (isPageBefore m x) ys

isRowGood :: OrderMap -> PageUpdate -> Bool
isRowGood _ [] = True
isRowGood m (x : xs) = isRowGood m xs && not (areAnyPagesEarly m x xs)

getMiddle :: PageUpdate -> Int
getMiddle xs = xs !! (length xs `div` 2)

----------- PART A -------------

partA :: Input -> OutputA
partA (rules, pages) = sum $ map (getMiddle . fst) $ filter snd $ zip pages (map (isRowGood (combineKeys $ map swap rules)) pages)

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
