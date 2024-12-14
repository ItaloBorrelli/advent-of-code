module Util.Days
    ( days
    )
where

--- Day imports
import qualified AOC.Y2022.Day01 as Y2022Day01
import qualified AOC.Y2022.Day02 as Y2022Day02
import qualified AOC.Y2024.Day01 as Y2024Day01
import qualified AOC.Y2024.Day02 as Y2024Day02
import qualified AOC.Y2024.Day03 as Y2024Day03
import qualified AOC.Y2024.Day04 as Y2024Day04
import qualified AOC.Y2024.Day05 as Y2024Day05
import qualified AOC.Y2024.Day06 as Y2024Day06
import qualified AOC.Y2024.Day07 as Y2024Day07
import qualified AOC.Y2024.Day08 as Y2024Day08
import qualified AOC.Y2024.Day09 as Y2024Day09
import qualified AOC.Y2024.Day10 as Y2024Day10
import qualified AOC.Y2024.Day11 as Y2024Day11
import qualified AOC.Y2024.Day12 as Y2024Day12
import qualified AOC.Y2024.Day13 as Y2024Day13
import qualified AOC.Y2024.Day14 as Y2024Day14

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
      (202201, (Y2022Day01.runDay, "inputs/2022/01/input.txt")),
      (202202, (Y2022Day02.runDay, "inputs/2022/02/input.txt")),
      (202401, (Y2024Day01.runDay, "inputs/2024/01/input.txt")),
      (202402, (Y2024Day02.runDay, "inputs/2024/02/input.txt")),
      (202403, (Y2024Day03.runDay, "inputs/2024/03/input.txt")),
      (202404, (Y2024Day04.runDay, "inputs/2024/04/input.txt")),
      (202405, (Y2024Day05.runDay, "inputs/2024/05/input.txt")),
      (202406, (Y2024Day06.runDay, "inputs/2024/06/input.txt")),
      (202407, (Y2024Day07.runDay, "inputs/2024/07/input.txt")),
      (202408, (Y2024Day08.runDay, "inputs/2024/08/input.txt")),
      (202409, (Y2024Day09.runDay, "inputs/2024/09/input.txt")),
      (202410, (Y2024Day10.runDay, "inputs/2024/10/input.txt")),
      (202411, (Y2024Day11.runDay, "inputs/2024/11/input.txt")),
      (202412, (Y2024Day12.runDay, "inputs/2024/12/input.txt")),
      (202413, (Y2024Day13.runDay, "inputs/2024/13/input.txt")),
      (202414, (Y2024Day14.runDay, "inputs/2024/14/input.txt"))
    ]
{- FOURMOLU_ENABLE -}