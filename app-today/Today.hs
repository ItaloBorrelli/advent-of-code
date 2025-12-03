{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use empty" #-}

-- Data Output

import AOC.Y2025.Day03 qualified as Y2025Day03
import Control.Applicative.Combinators qualified as C (option)
import Data.Maybe (fromMaybe)
import Options.Applicative
    ( Alternative ((<|>))
    , Parser
    , execParser
    , flag'
    , fullDesc
    , help
    , helper
    , info
    , long
    , metavar
    , optional
    , progDesc
    , short
    , strOption
    , (<**>)
    )
import Program.Color (withColor)
import Program.RunDay
    ( Day
    , Verbosity (Quiet, Timings, Verbose)
    )
import System.Console.ANSI (Color (..))
import Text.Printf (printf)

newtype Days = OneDay
    { input :: Maybe String
    }
    deriving (Show)

data Options = Options Days Verbosity

dayParser :: Options.Applicative.Parser Days
dayParser =
    OneDay
        <$> Options.Applicative.optional
            ( Options.Applicative.strOption $
                Options.Applicative.long "input"
                    <> Options.Applicative.short 'i'
                    <> Options.Applicative.metavar "FILE"
                    <> Options.Applicative.help "The file to read the selected day's input from."
            )

optionsParser :: Options.Applicative.Parser Options
optionsParser = Options <$> dayParser <*> verbosityParser
  where
    verbosityParser :: Options.Applicative.Parser Verbosity
    verbosityParser =
        C.option Quiet $
            Options.Applicative.flag'
                Verbose
                ( Options.Applicative.long "verbose"
                    <> Options.Applicative.short 'v'
                    <> Options.Applicative.help
                        ( unwords
                            [ "Whether to print out extra info, such as the"
                            , "result of the input parser, and more detailed"
                            , "error messages."
                            , "Also enables timing of solutions."
                            ]
                        )
                )
                Options.Applicative.<|> Options.Applicative.flag'
                    Timings
                    ( Options.Applicative.long "timings"
                        <> Options.Applicative.short 't'
                        <> Options.Applicative.help
                            ( unwords
                                ["Whether to enable timing of the solutions."]
                            )
                    )

formatDay :: Int -> String
formatDay d = printf "\n***Year %s Day %02d***" (take 4 (show d)) (d `mod` 100)

days :: (Int, Day, String)
days =
    (3, Y2025Day03.runDay, "inputs/2025/03/input.txt")

performDay :: Options -> IO ()
performDay (Options (OneDay {..}) v) =
    do
        let
            (day, dayFunc, inputFile) = days
            i' = fromMaybe inputFile input
        withColor Magenta $ putStrLn $ formatDay day
        _ <- dayFunc v i'
        withColor Magenta $ putStrLn "************"

main :: IO ()
main = performDay =<< Options.Applicative.execParser opts
  where
    opts =
        Options.Applicative.info
            (optionsParser Options.Applicative.<**> Options.Applicative.helper)
            (Options.Applicative.fullDesc <> Options.Applicative.progDesc "Prints out some Advent of Code solutions.")
