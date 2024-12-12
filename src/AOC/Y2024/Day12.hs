module AOC.Y2024.Day12 (runDay) where

import Data.Void ( Void )
import qualified Program.RunDay   as R (Day, runDay)
import Text.Parsec ( newline, sepBy, eof, many )
import Text.Parsec.Text ( Parser )
import Text.Parsec.Char (noneOf)
import Data.Map.Strict (Map, (!?), (!), insertWith, foldlWithKey, empty, insert, mapAccumWithKey, keys)
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

type M = Map C (Bool, Char)

----------- PARSER -------------

inputParser :: Parser M
inputParser = mapFromNestedLists . map (map (False,)) <$> many (noneOf "\n") `sepBy` newline <* eof

----------- PART A&B -----------

up, down, right, left :: C -> C
up = first (\x -> x - 1)
down = first (+ 1)
left = second (\x -> x - 1)
right = second (+ 1)

check :: M -> Char -> (P, [C]) -> C -> (P, [C])
check m ch (p, n) c' = case m !? c' of
    Nothing -> (p + 1, n)
    Just (v, ch') -> (if ch /= ch' then p + 1 else p, if ch == ch' && not v then c':n else n)

borderSame :: M -> C -> Char -> (P, [C])
borderSame m c ch = foldl (check m ch) (0, []) $ map ($ c) [up, down, left, right]

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (a, b) (c, d) = (a + c, b + d)

checkArea :: M -> (P, A) -> C -> Char -> (M, (P, A))
checkArea m (p, a) c ch =
    let m' = insert c (True, ch) m
        (p', cs) = borderSame m' c ch
    in foldl (\(m'', (p'', a')) c' -> if not (fst (m'' ! c')) then checkArea m'' (p'', a') c' ch else (m'', (p'', a'))) (m', (p + p', a + 1)) cs


----------- PART A -------------

partA :: Input -> OutputA
partA m = fst $ foldl (\(total, m') k ->
    let (v, ch) = (m' ! k)
        (m'', (p, a)) = if not v then checkArea m' (0, 0) k ch else (m', (0, 0))
    in trace (show (ch, k, p, a)) (total + (p * a), m'')) (0, m) (keys m)

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
