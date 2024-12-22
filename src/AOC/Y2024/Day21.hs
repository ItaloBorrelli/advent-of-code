module AOC.Y2024.Day21 (runDay) where

import Data.Bifunctor (first, second)
import Data.Map.Lazy (Map, (!))
import Data.Map.Lazy qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void ()
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, newline, sepBy)
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = [(Char, Char, Char)]

type OutputA = Int

type OutputB = Int

type Coordinate = (Int, Int)

type PathSet = Set String

data DirPad = DA | DR | DL | DU | DD deriving (Eq, Ord, Show)

data NumPad = NA | N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 deriving (Eq, Ord, Show)

----------- PARSER -------------

inputParser :: Parser Input
inputParser = ((,,) <$> digit <*> digit <*> digit <* char 'A') `sepBy` newline

----------- PART A&B -----------

numpad :: Map Char Coordinate
numpad =
    M.fromList
{- FOURMOLU_DISABLE -}
        [ ('7', (0, 0)), ('8', (1, 0)), ('9', (2, 0))
        , ('4', (0, 1)), ('5', (1, 1)), ('6', (2, 1))
        , ('1', (0, 2)), ('2', (1, 2)), ('3', (2, 2))
                       , ('0', (1, 3)), ('A', (2, 3))
        ]
{- FOURMOLU_ENABLE -}

go :: Char -> Coordinate -> Coordinate
go 'v' = second (1 +)
go '^' = second (\y -> y - 1)
go '>' = first (1 +)
go '<' = first (\x -> x - 1)
go _ = error "failure condition"

dirpad :: Map Char Coordinate
dirpad =
    M.fromList
{- FOURMOLU_DISABLE -}
                       [ ('^', (1, 0)), ('A', (2, 0))
        , ('<', (0, 1)), ('v', (1, 1)), ('>', (2, 1))
        ]
{- FOURMOLU_ENABLE -}

paths :: (Ord a) => Map a Coordinate -> a -> a -> PathSet
paths m s t =
    let
        ((sx, sy), (tx, ty)) = (m ! s, m ! t)
        h = replicate (abs (tx - sx)) (if tx > sx then '>' else '<')
        v = replicate (abs (ty - sy)) (if ty > sy then 'v' else '^')
     in
        S.fromList [h ++ v ++ "A", v ++ h ++ "A"]

filterPaths :: Map Char Coordinate -> Coordinate -> ((Char, Char), PathSet) -> ((Char, Char), PathSet)
filterPaths m badCoord (cs@(s, _), set) =
    let
        start = m ! s
     in
        (cs, S.filter (not . badPath start) set)
  where
    badPath :: Coordinate -> String -> Bool
    badPath _ [] = False
    badPath c (x : xs) =
        let
            next = go x c
         in
            (c == badCoord) || badPath next xs

numNav :: Map (Char, Char) PathSet
numNav =
    let
        ks = M.keys numpad
     in
        M.fromList [filterPaths numpad (0, 3) ((k0, k1), paths numpad k0 k1) | k0 <- ks, k1 <- ks]

dirNav :: Map (Char, Char) PathSet
dirNav =
    let
        ks = M.keys dirpad
     in
        M.fromList [filterPaths dirpad (0, 0) ((k0, k1), paths dirpad k0 k1) | k0 <- ks, k1 <- ks]


joino :: PathSet -> PathSet -> PathSet
joino a = S.foldr (\b acc -> S.union (S.map (++ b) a) acc) S.empty

-- If rob0 is being instructed by rob1 to go from s to t what are the possible paths
instructobot :: Map (Char, Char) PathSet -> String -> PathSet
instructobot m (s : t : rest) = joino (m ! (s, t)) (instructobot m (t : rest))
instructobot _ _ = S.singleton ""

minPresses :: PathSet -> Int
minPresses = minimum . S.map length

onlyBest :: PathSet -> PathSet
onlyBest bot = S.filter (\x -> length x == minPresses bot) bot

reinstruct :: PathSet -> PathSet
reinstruct x = S.unions (S.map (\y -> instructobot dirNav ('A' : y)) (onlyBest x))

----------- PART A -------------

partA :: Input -> OutputA
partA =
    sum
        . map
            ( \(a, b, c) ->
                let
                    bot0 = instructobot numNav ['A', a, b, c, 'A']
                    bot1 = reinstruct bot0
                    bot2 = reinstruct bot1
                    finalBest = minPresses bot2
                 in
                    read [a, b, c] * finalBest
            )

----------- PART B -------------

partB :: Input -> OutputB
partB =
    sum
        . map
            ( \(a, b, c) ->
                let
                    bot0 = instructobot numNav ['A', a, b, c, 'A']
                    bot1 = reinstruct bot0
                    bot2 = reinstruct bot1
                    finalBest = minPresses bot2
                 in
                    read [a, b, c] * finalBest
            )
