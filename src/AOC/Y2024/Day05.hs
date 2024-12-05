module AOC.Y2024.Day05 (runDay) where

import Data.Void ( Void )
import qualified Program.RunDay   as R (Day, runDay)
import Text.Parsec ( char, digit, newline, endBy, many1, sepBy, eof )
import Text.Parsec.Text ( Parser )
import Data.Map.Strict (Map, fromListWith, (!?))
import Control.Arrow (Arrow(second))
import Data.Tuple (swap)

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
combineKeys = fromListWith (++) . map (second (:[]))

-- is second int before the first int in ordering rules where the order rules are from the last page to the first page
isPageBefore :: OrderMap -> Int -> Int -> Bool
isPageBefore m current q  = case m !? current of
        Nothing -> False
        Just prevPages -> q `elem` prevPages -- if q is in prevPages then q is before current
            || any (\p -> isPageBefore m p q) prevPages

areAnyPagesEarly :: OrderMap -> Int -> [Int] -> Bool
areAnyPagesEarly _ _ [] = False
areAnyPagesEarly m x ys = any (isPageBefore m x) ys

isRowGood :: OrderMap -> PageUpdate -> Bool
isRowGood _ [] = True
isRowGood m (x:xs) = isRowGood m xs && not (areAnyPagesEarly m x xs)

getMiddle :: PageUpdate -> Int
getMiddle xs = xs !! (length xs `div` 2)

----------- PART A -------------

partA :: Input -> OutputA
partA (rules, pages) = sum $ map (getMiddle . fst) $ filter snd $ zip pages (map (isRowGood (combineKeys $ map swap rules)) pages)

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
