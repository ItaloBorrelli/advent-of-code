module AOC.Y2024.Day06 (runDay) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)
import Data.Map.Strict (Map, empty, insert, insertWith, (!?))
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, singleton, union)
import Data.String (IsString (..))
import Data.Tuple.Extra (both, third3)
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

type OutputB = Int

-- Coordinate
type C = (Int, Int)

-- Step
type S = C

-- Map of Coordinates to Spots
type M = Map C Spot

data D = Up | Do | Le | Ri deriving (Eq, Ord, Show)

-- Axis of Vertical or Horizontal
data A = V | H deriving (Eq, Ord)

-- Line of Axis and respectively:

-- | Horizontal -> the y value and the x value of last empty spot left<->right
-- | Vertical -> the x value and the y value of last empty spot up<->down
type L = Map (D, Int) (Set (Int, Int))

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
        Just Ob -> Just (turn d, c)
        Just _ -> Just (d, step)

findNextObstruction :: M -> C -> D -> C
findNextObstruction m c d =
  let c' = nextStep c d
   in case m !? c' of
        Nothing -> c
        Just Ob -> c
        _ -> findNextObstruction m c' d

onLine :: L -> C -> D -> Bool
onLine ls (x, y) d
  | d == Le || d == Ri = maybe False (any (\(x1, x2) -> x1 <= x && x2 >= x)) (ls !? (d, y))
  | otherwise = maybe False (any (\(y1, y2) -> y1 <= y && y2 >= y)) (ls !? (d, x))

recordLine :: M -> C -> D -> L -> L
recordLine m (x, y) d ls
  | d == Le || d == Ri = insertWith union (d, y) (singleton (both (fst . findNextObstruction m (x, y)) (Le, Ri))) ls
  | otherwise = insertWith union (d, x) (singleton (both (snd . findNextObstruction m (x, y)) (Up, Do))) ls

visit :: M -> L -> C -> D -> (Int, Int) -> (M, L, Int, Int)
visit m ls c d (v, o) =
  let (ls', o') = (recordLine m c d ls, o + fromEnum (onLine ls c (turn d)))
   in case m !? c of
        Just Vi -> (m, ls', v, o')
        _ -> (insert c Vi m, ls', v + 1, o')

doRounds :: M -> L -> C -> D -> (Int, Int) -> (Int, Int)
doRounds m ls c d (v, o) =
  let (m', ls', v', o') = visit m ls c d (v, o)
      dc' = whereTo m c d
   in case dc' of
        Nothing -> (v', o')
        Just (d', c') -> doRounds m' ls' c' d' (v', o')

visit' :: M -> L -> C -> D -> (M, L, Bool)
visit' m ls c d =
  let ls' = recordLine m c d ls
   in case m !? c of
        Just Vi -> (m, ls', False)
        _ -> (insert c Vi m, ls', True)

fakeRun :: M -> L -> C -> D -> Maybe L
fakeRun m ls c d =
  let ls' = recordLine m c d ls
      c' = findNextObstruction m c d
      obstacle = m !? nextStep c' d
   in if onLine ls c d
        then Just ls'
        else
          if isNothing obstacle
            then Nothing
            else fakeRun m ls' c' (turn d)

gallivant :: M -> L -> C -> D -> (Int, Int) -> (Int, Int)
gallivant m ls c d (v, o) =
  let (m', ls', v') = third3 ((+ v) . fromEnum) $ visit' m ls c d
      dc' = whereTo m c d
   in case dc' of
        Nothing -> (v', o)
        Just (d', c') ->
          let runResult = fakeRun (insert (nextStep c' d') Ob m) ls c (turn d)
              (ls'', o') = (if isNothing runResult then (ls', o) else (fromJust runResult, o + 1))
           in gallivant m' ls'' c' d' (v', o')

----------- PART A -------------

partA :: Input -> OutputA
partA (s, m) = fst $ doRounds m empty s Up (1, 0)

----------- PART B -------------

partB :: Input -> OutputB
partB (s, m) = snd $ gallivant m empty s Up (1, 0)
