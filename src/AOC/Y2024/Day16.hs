module AOC.Y2024.Day16 (runDay) where

import Data.Void ( Void )
import qualified Program.RunDay   as R (Day, runDay)
import Text.Parsec ( char, newline, eof, sepBy, (<|>), many )
import Text.Parsec.Text ( Parser )
import Data.Map (Map)
import Util.Util (mapFromNestedLists')
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type C = (Int, Int)

type R = C

type E = C

data X = W' | N' | S | E deriving (Eq, Show)

data Y = W | N deriving (Eq, Show)

type M = Map C Y

type Grid = [[X]]

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

type Input = (R, E, M)

type OutputA = Input

type OutputB = Void

----------- PARSER -------------

parseMapLine :: Parser [X]
parseMapLine = many (W' <$ char '#' <|> N' <$ char '.' <|> S <$ char 'S' <|> E <$ char 'E')

start :: Grid -> C
start = head . concatMap (\(y, row) -> [(y, x) | (x, v) <- zip [0 ..] row, v == S]) . zip [0 ..]

end :: Grid -> C
end = head . concatMap (\(y, row) -> [(y, x) | (x, v) <- zip [0 ..] row, v == E]) . zip [0 ..]

toMap :: Grid -> M
toMap = mapFromNestedLists' . map (map (\case W' -> W; _ -> N))

inputParser :: Parser Input
inputParser = (\g -> (start g, end g, toMap g)) <$> parseMapLine `sepBy` newline <* eof

----------- PART A&B -----------

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

----------- PART A -------------

partA :: Input -> OutputA
partA x = x

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
