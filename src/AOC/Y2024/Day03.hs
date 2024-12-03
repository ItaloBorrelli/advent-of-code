module AOC.Y2024.Day03 (runDay) where

import           Data.Either      (rights)
import           Data.Functor     (($>))
import           Data.Maybe       (catMaybes)
import           Data.Void
import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec
import           Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
parseAnyMul :: Parser (Int, Int)
parseAnyMul =
    string "mul(" *>
    (((,) . read <$> many1 digit)
         <*> (char ',' *> (read <$> many1 digit)))
    <* char ')'

parseAnyOrSkip :: Parser (Maybe (Int, Int))
parseAnyOrSkip =
    (Just <$> try parseAnyMul)
    <|> (anyChar $> Nothing)

inputParser :: Parser Input
inputParser = catMaybes <$> (many parseAnyOrSkip <* eof)

------------ TYPES -------------
type Input = [(Int, Int)]
-- type Input = [Either Char (Int, Int)]

type OutputA = Int
-- type OutputA = [Either Char (Int, Int)]

type OutputB = Void

------------ UTIL --------------

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (uncurry (*))
-- partA x = x

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
