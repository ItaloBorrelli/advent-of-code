module AOC.Y2024.Day05 (runDay) where

import Control.Arrow (Arrow (second))
import Data.List (sortBy)
import Data.Map.Strict (Map, fromListWith, (!?))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, endBy, many1, newline, sepBy)
import Text.Parsec.Text (Parser)
import Data.Aviary.Birds (goldfinch, cardinal)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Rule = (Int, Int)

type Page = [Int]

type RulesMap = Map Int [Int]

type SortResult = (Page, Bool)

type Input = ([Rule], [Page])

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

parseRule :: Parser Rule
parseRule = ((,) . read <$> many1 digit) <* char '|' <*> (read <$> many1 digit)

parsePages :: Parser [Page]
parsePages = filter (not . null) <$> ((read <$> many1 digit) `sepBy` char ',' `sepBy` newline)

inputParser :: Parser Input
inputParser = (,) <$> parseRule `endBy` newline <* newline <*> parsePages

----------- PART A&B -----------

combineKeys :: [Rule] -> RulesMap
combineKeys = fromListWith (++) . map (second (: []))

getMiddle :: Page -> Int
getMiddle xs = xs !! (length xs `div` 2)

fixPage :: RulesMap -> Page -> Page
fixPage m =
  sortBy
    ( \a b ->
        case m !? a of
          Just aNext | b `elem` aNext -> LT
          _ -> case m !? b of
            Just bNext | a `elem` bNext -> GT
            _ -> EQ
    )

fixAndRecordDiff :: RulesMap -> Page -> SortResult
fixAndRecordDiff m orig = (\result -> (result, result == orig)) $ fixPage m orig

avianPageSort :: [Rule] -> [Page] -> [SortResult]
avianPageSort = map . goldfinch (cardinal fixAndRecordDiff) combineKeys
-- (b -> c -> d) -> (a -> b) -> a -> c -> d

----------- PART A -------------

partA :: Input -> OutputA
partA (rules, pages) = sum $ map (getMiddle . fst) $ filter snd $ uncurry avianPageSort (rules, pages)

----------- PART B -------------

partB :: Input -> OutputB
partB (rules, pages) = sum $ map (getMiddle . fst) $ filter (not . snd) $ uncurry avianPageSort (rules, pages)
