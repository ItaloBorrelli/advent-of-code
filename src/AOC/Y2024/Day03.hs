module AOC.Y2024.Day03 (runDay) where

import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec      (anyChar, char, digit, eof, many, many1,
                                   string, try, (<|>))
import           Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
parseMul :: Parser (Int, Int)
parseMul =
    string "mul(" *>
    (((,) . read <$> many1 digit)
         <*> (char ',' *> (read <$> many1 digit)))
    <* char ')'

inputParser :: Parser Input
inputParser = many $ do
    Mul <$> try parseMul
    <|> (Enable <$ try (string "do()"))
    <|> (Disable <$ try (string "don't()"))
    <|> (Junk <$> anyChar)

------------ TYPES -------------
type Input = [Commands]

type OutputA = Int

type OutputB = Int

data Commands = Mul (Int, Int) | Junk Char | Enable | Disable deriving (Show)

------------ UTIL --------------
multMults :: Commands -> Int
multMults (Mul (a, b)) = a * b
multMults _            = 0

filterMults :: Bool -> [Commands] -> [(Int, Int)]
filterMults False (Enable:xs) = filterMults True xs
filterMults True (Disable:xs) = filterMults False xs
filterMults False (_:xs)      = filterMults False xs
filterMults True (Mul x:xs)   = x : filterMults True xs
filterMults True (_:xs)       = filterMults True xs
filterMults _ []              = []

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map multMults

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (uncurry (*)) . filterMults True
