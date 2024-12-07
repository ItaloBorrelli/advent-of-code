module AOC.Y2024.Day07 (runDay) where

import           Data.Void
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)
import Data.Map.Strict (Map, insertWith, (!), (!?))
import Data.Set (Set, singleton, union)
import Data.String (IsString (..))
import Program.RunDay qualified as R
    ( Day, runDay, Day, runDay, Day, runDay )
import Text.Parsec
    ( char,
      eof,
      many,
      newline,
      sepBy,
      digit,
      many1,
      space,
      anyChar,
      char,
      digit,
      many,
      many1,
      string,
      try,
      char,
      eof,
      many,
      newline,
      (<|>) )
import Text.Parsec.Text ( Parser, Parser, Parser, Parser )
import Util.Util ( mapFromNestedLists, safeTake, (!!?) )
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator ( sepEndBy, sepBy )
import Data.Tuple.Extra (both)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Input = String

type OutputA = Input

type OutputB = Void

----------- PARSER -------------

inputParser :: Parser Input
inputParser = fail "Parser not yet implemented!"

----------- PART A&B -----------

----------- PART A -------------

partA :: Input -> OutputA
partA x = x

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
