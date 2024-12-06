{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use empty" #-}

module Main (main) where

--- Day imports
import qualified AOC.Y2022.Day01 as Y2022Day01
import qualified AOC.Y2022.Day02 as Y2022Day02
import qualified AOC.Y2024.Day01 as Y2024Day01
import qualified AOC.Y2024.Day02 as Y2024Day02
import qualified AOC.Y2024.Day03 as Y2024Day03
import qualified AOC.Y2024.Day04 as Y2024Day04
import qualified AOC.Y2024.Day05 as Y2024Day05
import qualified AOC.Y2024.Day06 as Y2024Day06
-- Other imports
import qualified Control.Applicative.Combinators as C (option)
import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Data.Map
    ( Map
    , fromList
    , mapKeys
    , mapWithKey
    , toList
    , (!?)
    )
import Data.Maybe (fromMaybe)
import Options.Applicative
    ( Alternative ((<|>))
    , Parser
    , auto
    , execParser
    , flag'
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , short
    , strOption
    , (<**>)
    )
-- Data Output
import Program.Color (withColor)
import Program.RunDay
    ( Day
    , Verbosity (Quiet, Timings, Verbose)
    )
import System.Console.ANSI (Color (..))
import Text.Printf (printf)

data Days
    = AllDays
    | OneDay
        { day :: Int
        , input :: Maybe String
        }
    deriving (Show)

data Options = Options Days Verbosity

dayParser :: Parser Days
dayParser = (OneDay <$> day <*> input) <|> allDays
  where
    day =
        option auto $
            long "day"
                <> short 'd'
                <> metavar "DAY"
                <> help "Present the solutions for one day."

    input =
        optional $
            strOption $
                long "input"
                    <> short 'i'
                    <> metavar "FILE"
                    <> help "The file to read the selected day's input from."

    allDays =
        flag' AllDays $
            long "all-days"
                <> help
                    ( unwords
                        [ "Present solutions for all of the days of"
                        , "Advent of Code, with default input file names."
                        ]
                    )

optionsParser :: Parser Options
optionsParser = Options <$> dayParser <*> verbosityParser
  where
    verbosityParser :: Parser Verbosity
    verbosityParser =
        C.option Quiet $
            flag'
                Verbose
                ( long "verbose"
                    <> short 'v'
                    <> help
                        ( unwords
                            [ "Whether to print out extra info, such as the"
                            , "result of the input parser, and more detailed"
                            , "error messages."
                            , "Also enables timing of solutions."
                            ]
                        )
                )
                <|> flag'
                    Timings
                    ( long "timings"
                        <> short 't'
                        <> help
                            ( unwords
                                ["Whether to enable timing of the solutions."]
                            )
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
      (202406, (Y2024Day06.runDay, "inputs/2024/06/input.txt"))
    ]

formatDay :: Int -> String
formatDay d = printf "\n***Year %s Day %02d***" (take 4 (show d)) (d `mod` 100)

performDay :: Options -> IO ()
performDay (Options d v) = case d of
    AllDays -> do
        results <-
            let
                eachDay day (dayFunc, inputFile) = do
                    withColor Magenta $ putStrLn $ formatDay day
                    dayFunc v inputFile
             in
                sequence $ mapWithKey eachDay days

        printSummary results
    OneDay {..} -> case days !? day of
        Nothing -> putStrLn "Invalid day provided."
        Just (dayFunc, inputFile) -> do
            let
                i' = fromMaybe inputFile input
            withColor Magenta $ putStrLn $ formatDay day
            _ <- dayFunc v i'
            withColor Magenta $ putStrLn "************"

printSummary :: Map Int (Maybe Double, Maybe Double) -> IO ()
printSummary results = do
    putStrLn "\n************\n  Summary:  "
    let
        partsA = mapKeys ((++ " (a)") . printf "%02d") $ fmap fst results
        partsB = mapKeys ((++ " (b)") . printf "%02d") $ fmap snd results
        parts = toList $ partsA <> partsB

        fails = [p | (p, Nothing) <- parts]
        fasts = [(p, t) | (p, Just t) <- parts, t < 1]
        slows = [(p, t) | (p, Just t) <- parts, t >= 1]

    putStr $ printf "\n%d parts " $ length fasts
    withColor Green $ putStr "completed in under 1 second"
    putStrLn ".\nOf the remainder:"
    unless (null fails) $ do
        putStr $ printf "  %d parts" $ length fails
        withColor Red $ putStr " failed"
        putStrLn $ ":\n    " ++ intercalate ", " fails
    unless (null slows) $ do
        putStr $ printf "  %d parts" $ length slows
        withColor Yellow $ putStr " took over 1 second to complete"
        putStrLn ":"
        forM_ slows $
            \(p, t) -> putStrLn $ printf "    %s took %.2f seconds" p t

main :: IO ()
main = performDay =<< execParser opts
  where
    opts =
        info
            (optionsParser <**> helper)
            (fullDesc <> progDesc "Prints out some Advent of Code solutions.")
