{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module AOC.Y2024.Day06 (runDay) where

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (transpose)
import Data.Map.Lazy (Map, empty, insert, insertWith, member, (!?))
import Data.Set (Set, singleton, union)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many1, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists)
import Data.Maybe (fromMaybe)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

-- Coordinate
type C = (Int, Int)

-- Direction
data D = U | D | L | R deriving (Eq, Ord, Show)

-- Guard
type G = (C, D)

-- O: Obstruction
-- N: Empty
data Spot = O | N deriving (Eq, Show)

data Start = Start Spot | Caret deriving (Eq)

type Grid = [[Start]]

-- Map of Coordinates to Spots
type M = Map C Spot

-- Map of Coordinates to a set of Directions
type V = Map C (Set D)

type Input = (G, M)

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

start :: Grid -> G
start = head . concatMap (\(y, row) -> [((x, y), U) | (x, spot) <- zip [0 ..] row, spot == Caret]) . zip [0 ..]

toMap :: Grid -> M
toMap = mapFromNestedLists . transpose . map (map (\s -> case s of Start spot -> spot; Caret -> N))

inputParser :: Parser Input
inputParser =
  (\x -> (start x, toMap x))
    <$> many1 (Caret <$ char '^' <|> Start O <$ char '#' <|> Start N <$ char '.') `sepBy` newline
    <* eof

----------- PART A&B -----------

turn :: D -> D
turn D = L
turn L = U
turn U = R
turn R = D

move :: G -> C
move (c, D) = second (1 +) c
move (c, L) = first (flip (-) 1) c
move (c, U) = second (flip (-) 1) c
move (c, R) = first (1 +) c

moveOrTurn :: M -> G -> Maybe G
moveOrTurn m g = case m !? move g of
  Nothing -> Nothing
  Just O -> Just (second turn g)
  Just N -> Just (move g, snd g)

vecsert :: G -> V -> V
vecsert (c, d) = insertWith union c (singleton d)

isNew :: C -> V -> Bool
isNew c = not . member c

----------- PART A -------------

visit :: G -> V -> Int
visit (c, _) v = fromEnum (isNew c v)

patrol' :: G -> V -> M -> Int
patrol' g v m = visit g v + case moveOrTurn m g of
  Nothing -> 0
  Just g' -> patrol' g' (vecsert g v) m

partA :: Input -> OutputA
partA (g, m) = patrol' g empty m

----------- PART B -------------

isRepeat :: G -> V -> Bool
isRepeat (c, d) v = fromMaybe False (v !? c >>= \ds -> Just (d `elem` ds))

loopCheck :: G -> V -> M -> Bool
loopCheck g v m = case moveOrTurn m g of
  Nothing -> False
  Just g' -> isRepeat g' v || loopCheck g' (vecsert g v) m

tryLoop :: G -> G -> V -> M -> Int
tryLoop g (c', _) v m = fromEnum (isNew c' v && loopCheck g v (insert c' O m))

gallivant :: G -> V -> M -> Int
gallivant g v m = case moveOrTurn m g of
  Nothing -> 0
  Just g' -> tryLoop g g' v m + gallivant g' (vecsert g v) m

partB :: Input -> OutputB
partB (g, m) = gallivant g empty m
