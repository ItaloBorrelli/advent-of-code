module AOC.Y2024.Day15 (runDay) where

import           Data.Void (Void)
import qualified Program.RunDay   as R (Day, runDay)
import           Text.Parsec (char, (<|>), many, sepBy, newline)
import           Text.Parsec.Text (Parser)
import Data.Map (Map, insert)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type A = C

data X = O | W | N deriving (Eq, Show)

data D = U | R | D | L deriving (Eq, Show)

type M = Map C X

type Input = ([[X]], [D])

type OutputA = Void

type OutputB = Void

----------- PARSER -------------

parseMapLine :: Parser [X]
parseMapLine = many (O <$ char 'O' <|> W <$ char '#' <|> N <$ char '.')

parseDirections :: Parser [D]
parseDirections = many (U <$ char '^' <|> R <$ char '>' <|> D <$ char 'v' <|> L <$ char '<')

inputParser :: Parser Input
inputParser =  <$ parseMapLine `sepBy` newline <* newline <*> parseDirections
----------- PART A&B -----------

----------- PART A -------------

partA :: Input -> OutputA
partA = error "Not implemented yet!"

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
