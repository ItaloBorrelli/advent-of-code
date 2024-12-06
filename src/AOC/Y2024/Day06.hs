module AOC.Y2024.Day06 (runDay) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)
import Data.Map.Strict (Map, insert, (!?))
import Data.String (IsString (..))
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

data Spot = V | O | E deriving (Eq)

instance Show Spot where
  show :: Spot -> String
  show O = "#"
  show E = "."
  show V = "X"

instance IsString Spot where
  fromString :: String -> Spot
  fromString "#" = O
  fromString "." = E
  fromString _ = V

type Input = [[Spot]]

type OutputA = Int

type OutputB = Void

type Coord = (Int, Int)

type Move = Coord

type Chizu = Map Coord Spot

data Dir = U | D | L | R deriving (Show)

----------- PARSER -------------

parseLine :: Parser [Spot]
parseLine = many ((V <$ char '^') <|> (E <$ char '.') <|> (O <$ char '#'))

inputParser :: Parser Input
inputParser = filter (not . null) <$> (parseLine `sepBy` newline) <* eof

----------- PART A&B -----------

findStartInList :: [[Spot]] -> [(Int, Int)]
findStartInList = concatMap (\(y, row) -> [(x, y) | (x, spot) <- zip [0 ..] row, spot == V]) . zip [0 ..]

move :: Dir -> Move
move U = (0, -1)
move D = (0, 1)
move R = (1, 0)
move L = (-1, 0)

turn :: Dir -> Dir
turn L = U
turn U = R
turn R = D
turn D = L

whereTo :: Chizu -> (Dir, Coord) -> Maybe (Dir, Coord)
whereTo m (d, curr) =
  let nextStep = bimap (+ fst curr) (+ snd curr) (move d)
   in case m !? nextStep of
        Nothing -> Nothing
        Just O -> whereTo m (turn d, curr)
        Just _ -> Just (d, nextStep)

updateMap :: Chizu -> Coord -> (Chizu, Bool)
updateMap m c = case m !? c of
  Just V -> (m, False)
  _ -> (insert c V m, True)

doRounds :: Chizu -> (Dir, Coord) -> Int -> Int
doRounds m a n =
  let step = whereTo m a
   in case step of
        Nothing -> n
        Just (newDir, next) ->
          let (newMap, moved) = updateMap m next
           in n + doRounds newMap (newDir, next) (fromEnum moved)

----------- PART A -------------

partA :: Input -> OutputA
partA input =
  let curr = head $ findStartInList input
      (chizu, _) = updateMap (mapFromNestedLists $ transpose input) curr
   in doRounds chizu (U, curr) 1

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
