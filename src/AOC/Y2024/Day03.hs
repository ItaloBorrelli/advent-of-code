module AOC.Y2024.Day03 (runDay) where

import           Data.Either      (rights)
import           Data.Void
import           Debug.Trace      (trace)
import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec
import           Text.Parsec      (many1, string)
import           Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
parseAnyMul :: Parser (Int, Int)
parseAnyMul = do
    _ <- string "mul("
    digits1 <- many1 digit
    _ <- char ','
    digits2 <- many1 digit
    _ <- char ')'
    return (read digits1, read digits2)

parseAnyOrSkip :: Parser (Either Char (Int, Int))
parseAnyOrSkip =
    (Right <$> try parseAnyMul)
    <|> (Left <$> anyChar)

inputParser :: Parser Input
inputParser = rights <$> (many parseAnyOrSkip <* eof)

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
