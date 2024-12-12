module AOC.Y2024.Day12 (runDay) where

import Data.Void ( Void )
import qualified Program.RunDay   as R (Day, runDay)
import Text.Parsec ( newline, sepBy, eof, many )
import Text.Parsec.Text ( Parser )
import Text.Parsec.Char (noneOf)
import Data.Map.Strict (Map, (!?), insertWith, foldlWithKey, empty)
import Data.Bifunctor (Bifunctor(second, first))
import Util.Util (mapFromNestedLists)
import qualified Data.Map.Strict as M (foldl)
import Debug.Trace (trace)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = M

type OutputA = Int

type OutputB = Void

type C = (Int, Int)

type P = Int

type A = Int

type PA = Map Char (P, A)

type M = Map C Char

----------- PARSER -------------

inputParser :: Parser M
inputParser = mapFromNestedLists <$> many (noneOf "\n") `sepBy` newline <* eof

----------- PART A&B -----------

up, down, right, left :: C -> C
up = first (\x -> x - 1)
down = first (+ 1)
left = second (\x -> x - 1)
right = second (+ 1)

borderSame :: M -> C -> Char -> Int
borderSame m c ch = sum $ map (fromEnum . (\d -> Just ch /= m !? d c)) [up, down, left, right]

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (a, b) (c, d) = (a + c, b + d)

accum :: M -> PA -> C -> Char -> PA
accum m pa c ch =
    let p' = borderSame m c ch
    in insertWith addPairs ch (p', 1) pa

----------- PART A -------------

partA :: Input -> OutputA
partA m =
    let n = foldlWithKey (accum m) (empty :: PA) m
    in trace (show n) M.foldl (\x (p, a) -> x + (p * a)) 0 n

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
