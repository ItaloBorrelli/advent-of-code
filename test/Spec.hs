{-# LANGUAGE OverloadedStrings #-}

import Data.List (isInfixOf)
import System.Process (readProcess)
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Regex (mkRegex, subRegex)

-- Path utilities for test inputs and answers
testInputPath :: String -> String -> FilePath
testInputPath year day = "inputs/" ++ year ++ "/" ++ day ++ "/test-input.txt"

testAnswerPath :: String -> String -> FilePath
testAnswerPath year day = "inputs/" ++ year ++ "/" ++ day ++ "/test-answers.txt"

-- Function to run the main program with arguments and capture output
runDayMain :: String -> FilePath -> IO String
runDayMain day inputFile = do
    readProcess "stack" ["run", "--", "-d", day, "-i", inputFile] ""

stripAnsiString :: String -> String
stripAnsiString input = subRegex (mkRegex "\x1b\\[[0-9;]*m") input ""

-- Parse the output of the main program to extract answers
parseOutput :: String -> (String, String)
parseOutput output =
    let
        linesOfOutput = lines output
        partAOutput = extractOutput "Part A:" linesOfOutput
        partBOutput = extractOutput "Part B:" linesOfOutput
     in
        (partAOutput, partBOutput)

-- Extract output after a specific section label
extractOutput :: String -> [String] -> String
extractOutput label = head . tail . dropWhile (/= label)

runDay :: (String, String) -> IO ()
runDay (year, day) = do
    let
        inputFile = testInputPath year day
    expectedAnswers <- lines <$> readFile (testAnswerPath year day)
    output <- runDayMain (year ++ day) inputFile
    let
        cleanOutput = stripAnsiString output
    if "Parser not yet implemented!" `isInfixOf` cleanOutput
        then do
            putStrLn $ "Test for " ++ year ++ day ++ " reported \"Parser not yet implemented!\"."
        else do
            putStrLn output
            let
                (actualPartA, actualPartB) = parseOutput cleanOutput

            -- Compare results
            actualPartA `shouldBe` head expectedAnswers
            actualPartB `shouldBe` last expectedAnswers

main :: IO ()
main = hspec $ do
    describe "AoC" $ do
        -- Add new tests here
        it "202420" $ do runDay ("2024", "20")
        it "202201" $ do runDay ("2022", "01")
        it "202202" $ do runDay ("2022", "02")
        it "202401" $ do runDay ("2024", "01")
        it "202402" $ do runDay ("2024", "02")
        it "202403" $ do runDay ("2024", "03")
        it "202404" $ do runDay ("2024", "04")
        it "202405" $ do runDay ("2024", "05")
        it "202406" $ do runDay ("2024", "06")
        it "202407" $ do runDay ("2024", "07")
        it "202408" $ do runDay ("2024", "08")
        it "202409" $ do runDay ("2024", "09")
        it "202410" $ do runDay ("2024", "10")
        it "202411" $ do runDay ("2024", "11")
        it "202412" $ do runDay ("2024", "12")
        it "202413" $ do runDay ("2024", "13")
        it "202414" $ do runDay ("2024", "14")
        it "202415" $ do runDay ("2024", "15")
        it "202417" $ do runDay ("2024", "17")
        it "202419" $ do runDay ("2024", "19")
