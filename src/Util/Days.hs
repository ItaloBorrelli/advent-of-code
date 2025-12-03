{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use empty" #-}
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
import AOC.Y2024.Day15 qualified as Y2024Day15
import AOC.Y2024.Day17 qualified as Y2024Day17
import AOC.Y2024.Day19 qualified as Y2024Day19
import AOC.Y2024.Day20 qualified as Y2024Day20
import AOC.Y2025.Day01 qualified as Y2025Day01
import AOC.Y2025.Day02 qualified as Y2025Day02
import Data.Map
    ( Map
    , fromList
    )
import Program.RunDay
    ( Day
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
      (202414, (Y2024Day14.runDay, "inputs/2024/14/input.txt")),
      (202415, (Y2024Day15.runDay, "inputs/2024/15/input.txt")),
      (202417, (Y2024Day17.runDay, "inputs/2024/17/input.txt")),
      (202419, (Y2024Day19.runDay, "inputs/2024/19/input.txt")),
      (202420, (Y2024Day20.runDay, "inputs/2024/20/input.txt")),
      (202501, (Y2025Day01.runDay, "inputs/2025/01/input.txt")),
      (202502, (Y2025Day02.runDay, "inputs/2025/02/input.txt"))
    ]
{- FOURMOLU_ENABLE -}
