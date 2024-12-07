module AOC.Y2024.Day06 (runDay) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)
import Data.Map.Strict (Map, insertWith, (!), (!?))
import Data.Set (Set, singleton, union)
import Data.String (IsString (..))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, eof, many, newline, sepBy, (<|>))
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = (V, M)

type OutputA = Int

type OutputB = Int

-- Coordinate
type C = (Int, Int)

-- Step
type S = C

-- Direction
data D = Up | Do | Le | Ri deriving (Eq, Ord, Show)

-- Vector
type V = (C, D)

-- Vi: Visited
-- Ob: Obstruction
-- Em: Empty
data Spot = Vi (Set D) | Ob | Em | St deriving (Eq)

-- Map of Coordinates to Spots
type M = Map C Spot

instance Show Spot where
  show :: Spot -> String
  show (Vi _) = "X"
  show Ob = "#"
  show Em = "."
  show St = "X"

instance IsString Spot where
  fromString "X" = Vi (singleton Up)
  fromString "#" = Ob
  fromString "." = Em
  fromString _ = error "Invalid Spot string"

----------- PARSER -------------

getStart :: [[Spot]] -> [(Int, Int)]
getStart = concatMap (\(y, row) -> [(x, y) | (x, spot) <- zip [0 ..] row, spot == St]) . zip [0 ..]

parseLine :: Parser [Spot]
parseLine = many ((St <$ char '^') <|> (Em <$ char '.') <|> (Ob <$ char '#'))

combine :: Spot -> Spot -> Spot
combine (Vi a) (Vi b) = Vi (a `union` b)
combine (Vi a) _ = Vi a
combine _ old = old

vecsert :: V -> M -> M
vecsert (c, d) = insertWith combine c (Vi $ singleton d)

formInput :: [[Spot]] -> Input
formInput g =
  let (v, m) = ((head $ getStart g, Up), mapFromNestedLists $ transpose g)
   in (v, vecsert v m)

inputParser :: Parser Input
inputParser =
  (formInput . filter (not . null) <$> (parseLine `sepBy` newline)) <* eof

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

nextStep :: V -> C
nextStep (c, d) = bimap (+ fst c) (+ snd c) (move d)

whereTo :: V -> M -> Maybe V
whereTo (c, d) m =
  let c' = nextStep (c, d)
   in case m !? c' of
        Nothing -> Nothing
        Just Ob -> Just (c, turn d)
        Just _ -> Just (c', d)

findNextObstruction :: V -> M -> Maybe C
findNextObstruction (c, d) m =
  let c' = nextStep (c, d)
   in case m !? c' of
        Nothing -> Nothing
        Just Ob -> Just c
        _ -> findNextObstruction (c', d) m

checkVec :: V -> M -> Bool
checkVec (c, d) m = case m ! c of
  Vi ds -> d `elem` ds
  _ -> False

willLoop :: V -> M -> Bool
willLoop (c, d) m =
  case findNextObstruction (c, d) m of
    Nothing -> False
    Just c' -> checkVec (c', turn d) m || willLoop (c', turn d) (vecsert (c, d) m)

gallivant :: V -> M -> (Int, Int)
gallivant (c, d) m =
  let isNew = (\case Just (Vi _) -> 0; Nothing -> 0; _ -> 1) (m !? c)
      v' = whereTo (c, d) m
      m' = vecsert (c, d) m
   in case v' of
        Nothing -> (isNew, 0)
        Just v'' ->
          let loops = fromEnum $ willLoop (c, turn d) (vecsert v'' m)
           in bimap (isNew +) (loops +) (gallivant v'' m')

----------- PART A -------------

partA :: Input -> OutputA
partA = (1 +) . fst . uncurry gallivant

----------- PART B -------------

partB :: Input -> OutputB
partB = snd . uncurry gallivant
