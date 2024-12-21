module AOC.Y2024.Day21 (runDay) where

import Data.Bifunctor (first, second)
import Data.List (permutations)
import Data.Map.Lazy (Map, (!))
import Data.Map.Lazy qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [(Char, Char, Char)]

type OutputA = Input

type OutputB = Void

type C = (Int, Int)

type A = Bool

----------- PARSER -------------

inputParser :: Parser Input
inputParser = ((\a b c -> (a, b, c)) <$> digit <*> digit <*> digit <* char 'A') `sepBy` newline

----------- PART A&B -----------

numpad :: Map Char C
numpad =
    M.fromList
{- FOURMOLU_DISABLE -}
        [ ('7', (0, 0)), ('8', (1, 0)), ('9', (2, 0))
        , ('4', (0, 1)), ('5', (1, 1)), ('6', (2, 1))
        , ('1', (0, 2)), ('2', (1, 2)), ('3', (2, 2))
                       , ('0', (1, 3)), ('A', (2, 3))
        ]
{- FOURMOLU_ENABLE -}

go :: Char -> C -> C
go 'v' = second (1 +)
go '^' = second (\y -> y - 1)
go '>' = first (1 +)
go '<' = first (\x -> x - 1)
go _ = error "failure condition"

dirpad :: Map Char C
dirpad =
    M.fromList
{- FOURMOLU_DISABLE -}
                       [ ('^', (1, 0)), ('A', (2, 0))
        , ('<', (0, 1)), ('v', (1, 1)), ('>', (2, 1))
        ]
{- FOURMOLU_ENABLE -}

paths :: (Eq a, Ord a) => Map a C -> a -> a -> Set [Char]
paths m s t =
    let
        ((sx, sy), (tx, ty)) = (m ! s, m ! t)
        h = replicate (abs (tx - sx)) (if tx > sx then '>' else '<')
        v = replicate (abs (ty - sy)) (if ty > sy then 'v' else '^')
     in
        S.fromList (permutations (h ++ v))

paths' :: (Eq a, Ord a) => Map a C -> a -> a -> Set [Char]
paths' m s t =
    let
        ((sx, sy), (tx, ty)) = (m ! s, m ! t)
        h = replicate (abs (tx - sx)) (if tx > sx then '>' else '<')
        v = replicate (abs (ty - sy)) (if ty > sy then 'v' else '^')
     in
        S.fromList [h ++ v ++ "A", v ++ h ++ "A"]

filterPaths :: Map Char C -> C -> ((Char, Char), Set [Char]) -> ((Char, Char), Set [Char])
filterPaths m badCoord (cs@(s, _), set) =
    let
        start = m ! s
     in
        (cs, S.filter (not . badPath start) set)
  where
    badPath :: C -> [Char] -> Bool
    badPath _ [] = False
    badPath c (x : xs) =
        let
            next = go x c
         in
            (c == badCoord) || badPath next xs

numNav' :: Map (Char, Char) (Set [Char])
numNav' =
    let
        ks = M.keys numpad
     in
        M.fromList [filterPaths numpad (0, 3) ((k0, k1), paths numpad k0 k1) | k0 <- ks, k1 <- ks]

dirNav' :: Map (Char, Char) (Set [Char])
dirNav' =
    let
        ks = M.keys dirpad
     in
        M.fromList [filterPaths dirpad (0, 0) ((k0, k1), paths dirpad k0 k1) | k0 <- ks, k1 <- ks]

numNav :: Map (Char, Char) (Set [Char])
numNav =
    let
        ks = M.keys numpad
     in
        M.fromList [filterPaths numpad (0, 3) ((k0, k1), paths' numpad k0 k1) | k0 <- ks, k1 <- ks]

dirNav :: Map (Char, Char) SC
dirNav =
    let
        ks = M.keys dirpad
     in
        M.fromList [filterPaths dirpad (0, 0) ((k0, k1), paths' dirpad k0 k1) | k0 <- ks, k1 <- ks]

type SC = Set [Char]

joino :: SC -> SC -> SC
joino a = S.foldr (\b acc -> S.union (S.map (++ b) a) acc) S.empty

-- If rob0 is being instructed by rob1 to go from s to t what are the possible paths
instructobot :: Map (Char, Char) SC -> [Char] -> SC
instructobot m (s : t : rest) = joino (m ! (s, t)) (instructobot m (t : rest))
instructobot m [s] = m ! (s, 'A')
instructobot _ _ = S.empty

----------- PART A -------------

partA :: Input -> Int
partA =
    sum
        . map
            ( \(a, b, c) ->
                let
                    bot0 = instructobot numNav ('A' : [a, b, c])
                    bot1 = reinstruct bot0
                    bot2 = reinstruct bot1
                    best = minimum $ S.map length bot2
                 in
                    read [a, b, c] * (best -2 )
            )
            where
                reinstruct :: SC -> SC
                reinstruct x = S.unions (S.map (\y -> instructobot dirNav ('A' : y)) x)

----------- PART B -------------

partB :: Input -> Int
partB =
    sum
        . map
            ( \(a, b, c) ->
                let
                    bot0 = instructobot numNav ('A' : [a, b, c])
                    bot1 = reinstruct bot0
                    bot2 = reinstruct bot1
                    best = minimum $ S.map length (reinstruct bot2)
                 in
                    read [a, b, c] * (best -2 )
            )
            where
                reinstruct :: SC -> SC
                reinstruct x = S.unions (S.map (\y -> instructobot dirNav ('A' : y)) x)
