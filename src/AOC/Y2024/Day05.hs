module AOC.Y2024.Day05 (runDay) where

import Control.Arrow (Arrow (second))
import Data.List (sortBy)
import Data.Map.Strict (Map, fromListWith, (!?))
import Data.Maybe (isJust, isNothing)
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

type OutputB = Int

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

getMiddle :: PageUpdate -> Int
getMiddle xs = xs !! (length xs `div` 2)

fixPage :: OrderMap -> PageUpdate -> PageUpdate
fixPage m =
  sortBy
    ( \a b ->
        case m !? a of
          Just aNext | b `elem` aNext -> LT
          _ -> case m !? b of
            Just bNext | a `elem` bNext -> GT
            _ -> EQ
    )

fixAndRecordDiff :: OrderMap -> PageUpdate -> (PageUpdate, Bool)
fixAndRecordDiff m orig = (\result -> (result, result == orig)) $ fixPage m orig

----------- PART A -------------

partA :: Input -> OutputA
partA (rules, pages) = sum $ map (getMiddle . fst) $ filter snd $ map (fixAndRecordDiff (combineKeys rules)) pages

----------- PART B -------------

partB :: Input -> OutputB
partB (rules, pages) = sum $ map (getMiddle . fst) $ filter (not . snd) $ map (fixAndRecordDiff (combineKeys rules)) pages
