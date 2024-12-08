module AOC.Y2024.Day09 (runDay) where

import Control.Arrow ((&&&))
import Data.Aviary.Birds (bluebird)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (on)
import Data.List (transpose)
import Data.Map.Strict
  ( Map,
    elems,
    empty,
    insertWith,
    (!),
    (!?),
  )
import Data.Maybe (fromMaybe)
import Data.Set (Set, singleton, union)
import Data.Set qualified as S
import Data.String (IsString (..))
import Data.Tuple.Extra (both)
import Data.Void
import Program.RunDay qualified as R
  ( Day,
    runDay,
  )
import Text.Parsec
  ( char,
    digit,
    eof,
    many,
    many1,
    newline,
    satisfy,
    sepBy,
    space,
    (<|>),
  )
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.Text (Parser)
import Util.Util
  ( allFoldl,
    mapFromNestedLists,
    tupleUp,
    twiceAsNice,
  )

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = Void

type OutputA = Void

type OutputB = Void

----------- PARSER -------------

inputParser :: Parser Input
inputParser = fail "Parser not yet implemented!"

----------- PART A&B -----------

----------- PART A -------------

partA :: Input -> OutputA
partA = error "Not implemented yet!"

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
