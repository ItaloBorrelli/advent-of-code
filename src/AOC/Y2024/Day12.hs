module AOC.Y2024.Day12 (runDay) where

import qualified Program.RunDay   as R (Day, runDay)
import Text.Parsec ( newline, sepBy, eof, many )
import Text.Parsec.Text ( Parser )
import Text.Parsec.Char (noneOf)
import Data.Map.Strict (Map, (!?), (!), insert, keys)
import Data.Bifunctor (Bifunctor(second, first))
import Util.Util (mapFromNestedLists)
import Debug.Trace (trace)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = M

type OutputA = Int

type OutputB = Int

type C = (Int, Int)

type P = Int

type A = Int

type V = Int

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

checkArea :: M -> (P, A) -> C -> Char -> (M, (P, A))
checkArea m (p, a) c ch =
    let m' = insert c (True, ch) m
        (p', cs) = borderSame m' c ch
    in foldl (\(m'', (p'', a')) c' -> if not (fst (m'' ! c')) then checkArea m'' (p'', a') c' ch else (m'', (p'', a'))) (m', (p + p', a + 1)) cs

valEq :: Char -> Maybe (Bool, Char) -> Bool
valEq ch v = case v of
    Nothing -> False
    Just (_, ch') -> ch == ch'

isVert :: M -> C -> Char -> (V, [C])
isVert m c ch =
    let m' = insert c (True, ch) m
        u = up c
        d = down c
        l = left c
        r = right c
        ul = left u
        ur = right u
        dl = left d
        dr = right d
        (_, cs) = foldl (check m ch) (0, []) $ map ($ c) [up, down, left, right]
        intCorners = sum $ map fromEnum [
            valEq ch (m' !? r) && valEq ch (m' !? d) && not (valEq ch (m' !? dr)),
            valEq ch (m' !? r) && valEq ch (m' !? u) && not (valEq ch (m' !? ur)),
            valEq ch (m' !? l) && valEq ch (m' !? d) && not (valEq ch (m' !? dl)),
            valEq ch (m' !? l) && valEq ch (m' !? u) && not (valEq ch (m' !? ul))]
        extCorners = sum $ map fromEnum [
            not ( valEq ch (m' !? r)) && not ( valEq ch (m' !? d)),
            not ( valEq ch (m' !? r)) && not ( valEq ch (m' !? u)),
            not ( valEq ch (m' !? l)) && not ( valEq ch (m' !? d)),
            not ( valEq ch (m' !? l)) && not ( valEq ch (m' !? u))]
    in (intCorners + extCorners, cs)

checkVert :: M -> (V, A) -> C -> Char -> (M, (V, A))
checkVert m (p, a) c ch =
    let m' = insert c (True, ch) m
        (p', cs) = isVert m' c ch
    in foldl (\(m'', (p'', a')) c' -> if not (fst (m'' ! c')) then checkVert m'' (p'', a') c' ch else (m'', (p'', a'))) (m', (p + p', a + 1)) cs

----------- PART A -------------

partA :: Input -> OutputA
partA m = fst $ foldl (\(total, m') k ->
    let (v, ch) = (m' ! k)
        (m'', (p, a)) = if not v then checkArea m' (0, 0) k ch else (m', (0, 0))
    in (total + (p * a), m'')) (0, m) (keys m)

----------- PART B -------------

partB :: Input -> OutputB
partB m = fst $ foldl (\(total, m') k ->
    let (v, ch) = (m' ! k)
        (m'', (p, a)) = if not v then checkVert m' (0, 0) k ch else (m', (0, 0))
    in trace (show (ch, k, p, a)) (total + (p * a), m'')) (0, m) (keys m)
--trace (show (ch, k, p, a))