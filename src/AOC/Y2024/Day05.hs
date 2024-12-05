module AOC.Y2024.Day05 (runDay) where

import Control.Arrow (Arrow (second))
import Data.List (sortBy)
import Data.Map.Strict (Map, fromListWith, (!?))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, endBy, many1, newline, sepBy, eof)
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
inputParser = (,) <$> parseRule `endBy` newline <* newline <*> parsePages <* eof

----------- PART A&B -----------

toMap :: [Rule] -> RulesMap
toMap = fromListWith (++) . map (second (: []))

middle :: Page -> Int
middle xs = xs !! (length xs `div` 2)

pageOrder :: RulesMap -> Int -> Int -> Ordering
pageOrder m a b = case m !? a of
          Just aNext | b `elem` aNext -> LT
          _ -> case m !? b of
            Just bNext | a `elem` bNext -> GT
            _ -> EQ

sortPage :: RulesMap -> Page -> Page
sortPage = sortBy . pageOrder 

sortAndCheck :: RulesMap -> Page -> SortResult
sortAndCheck m orig = (\result -> (result, result == orig)) $ sortPage m orig

avianPageSort :: [Rule] -> [Page] -> [SortResult]
avianPageSort = map . goldfinch (cardinal sortAndCheck) toMap

----------- PART A -------------

partA :: Input -> OutputA
partA = sum . map (middle . fst) . filter snd . uncurry avianPageSort

----------- PART B -------------

partB :: Input -> OutputB
partB = sum . map (middle . fst) . filter (not . snd) . uncurry avianPageSort
