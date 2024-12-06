module AOC.Y2024.Day06 (runDay) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)
import Data.Map.Strict (Map, (!?), empty,insert, insertWith)
import Data.String (IsString (..))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists)
import Data.Set (Set, singleton, union)
import Data.Tuple.Extra (both)

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

type OutputB = Int

-- Coordinate
type C = (Int, Int)

-- Step
type S = C

-- Map of Coordinates to Spots
type M = Map C Spot

data D = Up | Do | Le | Ri deriving (Show)

-- Axis of Vertical or Horizontal
data A = V | H deriving (Eq, Ord)

-- Line of Axis and respectively:
-- | Horizontal -> the y value and the x value of last empty spot left<->right
-- | Vertical -> the x value and the y value of last empty spot up<->down
type L = Map (A, Int) (Set (Int, Int))

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

nextStep :: C -> D -> C
nextStep c d = bimap (+ fst c) (+ snd c) (move d)

whereTo :: M -> C -> D -> Maybe (D, C)
whereTo m c d =
  let step = nextStep c d
   in case m !? step of
        Nothing -> Nothing
        Just Ob -> whereTo m c (turn d)
        Just _ -> Just (d, step)

findNextObstruction :: M -> C -> D -> C
findNextObstruction m c d =
  let c' = nextStep c d
   in case m !? c' of
        Nothing -> c
        Just Ob -> c
        _ -> findNextObstruction m c' d

getAxis :: D -> A
getAxis Le = H
getAxis Ri = H
getAxis Up = V
getAxis Do = V

onLine :: L -> C -> A -> Bool
onLine ls (x, y) H = maybe False (any (\(x1, x2) -> x1 <= x && x2 >= x)) (ls !? (H, y))
onLine ls (x, y) V = maybe False (any (\(x1, x2) -> x1 <= y && x2 >= y)) (ls !? (V, x))

recordLine :: M -> C -> A -> L -> L
recordLine m (x, y) H = insertWith union (H, y) (singleton (both (fst . findNextObstruction m (x, y)) (Le, Ri)))
recordLine m (x, y) V = insertWith union (V, x) (singleton (both (snd . findNextObstruction m (x, y)) (Up, Do)))

visit :: M -> L -> C -> D -> (Int, Int) -> (M, L, Int, Int)
visit m ls c d (v, o) = case m !? c of
  Just Vi -> (m, ls, v, o)
  _ -> (insert c Vi m, recordLine m c (getAxis d) ls, v + 1, o + fromEnum (onLine ls c (getAxis $ turn d)))

doRounds :: M -> L -> C -> D -> (Int, Int) -> (Int, Int)
doRounds m ls c d (v, o) =
  let (m', ls', v', o') = visit m ls c d (v, o)
      dc' = whereTo m c d
   in case dc' of
        Nothing -> (v', o')
        Just (d', c') -> doRounds m' ls' c' d' (v', o')

----------- PART A -------------

partA :: Input -> OutputA
partA (s, m) = fst $ doRounds m empty s Up (1, 0)

----------- PART B -------------

partB :: Input -> OutputB
partB (s, m) = snd $ doRounds m empty s Up (1, 0)
