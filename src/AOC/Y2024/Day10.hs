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

type PathFromCoord = Map C Int

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

findPaths :: PathFromCoord -> FullMap -> Maybe C -> Int -> (PathFromCoord, Int)
findPaths ps _ Nothing _ = (ps, 0)
findPaths ps m (Just c) 9 = if m !? c == Just 9 then (insert c 1 ps, 1) else (ps, 0)
findPaths ps m (Just c) v =
  case m !? c of
    Nothing -> (ps, 0)
    Just val ->
      if val /= v
        then (ps, 0)
        else case ps !? c of
          Just count -> (ps, count)
          Nothing ->
            let v' = v + 1
                (psUp, countUp) = findPaths ps m (Just (up c)) v'
                (psDown, countDown) = findPaths psUp m (Just (down c)) v'
                (psLeft, countLeft) = findPaths psDown m (Just (left c)) v'
                (psRight, countRight) = findPaths psLeft m (Just (right c)) v'
                total = countUp + countDown + countLeft + countRight
             in (insert c total psRight, total)

findNumRoutes :: Map C Bool -> FullMap -> Maybe C -> Int -> (Map C Bool, Int)
findNumRoutes ps _ Nothing _ = (ps, 0)
findNumRoutes ps m (Just c) 9 = if m !? c == Just 9 && ps !? c /= Just True then (insert c True ps, 1) else (ps, 0)
findNumRoutes ps m (Just c) v =
  case m !? c of
    Nothing -> (ps, 0)
    Just val ->
      if val /= v
        then (ps, 0)
        else
          let v' = v + 1
              (psUp, countUp) = findNumRoutes ps m (Just (up c)) v'
              (psDown, countDown) = findNumRoutes psUp m (Just (down c)) v'
              (psLeft, countLeft) = findNumRoutes psDown m (Just (left c)) v'
              (psRight, countRight) = findNumRoutes psLeft m (Just (right c)) v'
              total = countUp + countDown + countLeft + countRight
           in (psRight, total)

----------- PART A -------------

partA :: Input -> OutputA
partA g =
  let m = mapFromNestedLists g
      zs = indexZeroes g
   in foldl (\total z -> (+) total $ snd $ findNumRoutes empty m (Just z) 0) 0 zs

-- in snd $ foldl (\(pathMap, total) z -> second (total + ) $ findPaths pathMap m (Just z) 0) (empty, 0) zs

----------- PART B -------------

partB :: Input -> OutputB
partB g =
  let m = mapFromNestedLists g
      zs = indexZeroes g
   in snd $ foldl (\(pathMap, total) z -> second (total +) $ findPaths pathMap m (Just z) 0) (empty, 0) zs
