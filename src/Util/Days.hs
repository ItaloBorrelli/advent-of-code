{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use empty" #-}
module Util.Days
    ( days
    )
where

--- Day imports

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
    ]
{- FOURMOLU_ENABLE -}
