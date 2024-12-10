module AOC.Y2024.Day10 (runDay) where

import Data.Bifunctor (first, second)
import Data.Char (digitToInt)
import Data.Map.Lazy (Map, empty, insert, (!?))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
  ( digit,
    eof,
    many,
    newline,
    sepBy,
  )
import Text.Parsec.Text (Parser)
import Util.Util (mapFromNestedLists)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type FoundNines = Map C Bool

type CToPaths = Map C Int

type FullMap = Map C Int

type Input = [[Int]]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

inputParser :: Parser Input
inputParser = many (digitToInt <$> digit) `sepBy` newline <* eof

----------- PART A&B -----------

indexZeroes :: [[Int]] -> [C]
indexZeroes g = [(i, j) | (i, row) <- zip [0 ..] g, (j, x) <- zip [0 ..] row, x == 0]

up, down, left, right :: C -> C
up = first (\x -> x - 1)
down = first (+ 1)
left = second (\x -> x - 1)
right = second (+ 1)

----------- PART A -------------

findAccesses :: FoundNines -> FullMap -> Maybe C -> Int -> (FoundNines, Int)
findAccesses ps _ Nothing _ = (ps, 0)
findAccesses ps m (Just c) v =
  case m !? c of
    Nothing -> (ps, 0)
    Just h
      | h /= v -> (ps, 0)
      | h == 9 -> if ps !? c /= Just True then (insert c True ps, 1) else (ps, 0)
      | otherwise ->
          foldl
            ( \(ps', count') c' ->
                second (count' +) $
                  findAccesses ps' m c' (v + 1)
            )
            (ps, 0)
            $ map (\d -> Just (d c)) [up, down, left, right]

partA :: Input -> OutputA
partA g =
  let m = mapFromNestedLists g
      zs = indexZeroes g
   in foldl (\total z -> (+) total $ snd $ findAccesses empty m (Just z) 0) 0 zs

----------- PART B -------------

findTrails :: CToPaths -> FullMap -> Maybe C -> Int -> (CToPaths, Int)
findTrails ps _ Nothing _ = (ps, 0)
findTrails ps m (Just c) v =
  case m !? c of
    Nothing -> (ps, 0)
    Just h
      | h /= v -> (ps, 0)
      | h == 9 -> (insert c 1 ps, 1)
      | otherwise -> case ps !? c of
          Just count -> (ps, count)
          Nothing ->
            (\(ps', count') -> (insert c count' ps', count'))
              $ foldl
                ( \(ps', count') c' ->
                    second (count' +) $
                      findTrails ps' m c' (v + 1)
                )
                (ps, 0)
              $ map (\d -> Just (d c)) [up, down, left, right]

partB :: Input -> OutputB
partB g =
  let m = mapFromNestedLists g
      zs = indexZeroes g
   in snd $ foldl (\(pathMap, total) z -> second (total +) $ findTrails pathMap m (Just z) 0) (empty, 0) zs
