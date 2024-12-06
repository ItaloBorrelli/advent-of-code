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

-- Vi: Visited
-- Ob: Obstruction
-- Em: Empty
data Spot = Vi | Ob | Em deriving (Eq)

instance Show Spot where
  show :: Spot -> String
  show Ob = "#"
  show Em = "."
  show Vi = "X"

instance IsString Spot where
  fromString :: String -> Spot
  fromString "#" = Ob
  fromString "." = Em
  fromString _ = Vi

type Input = (C, M)

type OutputA = Int

type OutputB = Void

-- Coordinate
type C = (Int, Int)

-- Step
type S = C

-- Map of Coordinates to Spots
type M = Map C Spot

data D = Up | Do | Le | Ri deriving (Show)



----------- PARSER -------------

getStart :: [[Spot]] -> [(Int, Int)]
getStart = concatMap (\(y, row) -> [(x, y) | (x, spot) <- zip [0 ..] row, spot == Vi]) . zip [0 ..]

parseLine :: Parser [Spot]
parseLine = many ((Vi <$ char '^') <|> (Em <$ char '.') <|> (Ob <$ char '#'))

inputParser :: Parser Input
inputParser = ((\g -> (head $ getStart g, mapFromNestedLists $ transpose g)) . filter (not . null) <$> (parseLine `sepBy` newline)) <* eof

----------- PART A&B -----------

move :: D -> S
move Up = (0, -1)
move Do = (0, 1)
move Ri = (1, 0)
move Le = (-1, 0)

turn :: D -> D
turn Le = Up
turn Up = Ri
turn Ri = Do
turn Do = Le

nextStep :: D -> C -> C
nextStep d c = bimap (+ fst c) (+ snd c) (move d)

whereTo :: M -> D -> C -> Maybe (D, C)
whereTo m d curr =
  let step = nextStep d curr
   in case m !? step of
        Nothing -> Nothing
        Just Ob -> whereTo m (turn d) curr
        Just _ -> Just (d, step)

visit :: M -> C -> Int -> (M, Int)
visit m c v = case m !? c of
  Just Vi -> (m, v)
  _ -> (insert c Vi m, v + 1)

doRounds :: M -> D -> C -> Int -> Int
doRounds m d c v =
  let (m', v') = visit m c v
      step = whereTo m d c
   in case step of
        Nothing -> v'
        Just (d', c') -> doRounds m' d' c' v'

----------- PART A -------------

partA :: Input -> OutputA
partA (s, m) = doRounds m Up s 1

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
