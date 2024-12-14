module Util.Days
    ( days
    )
where

--- Day imports
import AOC.Y2022.Day01 qualified as Y2022Day01
import AOC.Y2022.Day02 qualified as Y2022Day02
import AOC.Y2024.Day01 qualified as Y2024Day01
import AOC.Y2024.Day02 qualified as Y2024Day02
import AOC.Y2024.Day03 qualified as Y2024Day03
import AOC.Y2024.Day04 qualified as Y2024Day04
import AOC.Y2024.Day05 qualified as Y2024Day05
import AOC.Y2024.Day06 qualified as Y2024Day06
import AOC.Y2024.Day07 qualified as Y2024Day07
import AOC.Y2024.Day08 qualified as Y2024Day08
import AOC.Y2024.Day09 qualified as Y2024Day09
import AOC.Y2024.Day10 qualified as Y2024Day10
import AOC.Y2024.Day11 qualified as Y2024Day11
import AOC.Y2024.Day12 qualified as Y2024Day12
import AOC.Y2024.Day13 qualified as Y2024Day13
import AOC.Y2024.Day14 qualified as Y2024Day14
import Data.Map
    ( Map
    , fromList
    , mapKeys
    , mapWithKey
    , toList
    , (!?)
    )
import Program.RunDay
    ( Day
    , Verbosity (Quiet, Timings, Verbose)
    )

{- FOURMOLU_DISABLE -}
days :: Map Int (Day, String)
days =
  fromList
    [ -- Insert new days here
      (202414, (Y2024Day14.runDay, "inputs/2024/14/input.txt"))
    ]
{- FOURMOLU_ENABLE -}
