module AOC.Y2024.Day19 (runDay) where

import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (string, many, char, sepBy)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (anyChar, satisfy)
import Text.Parsec (newline)
import Data.List (stripPrefix)
import Debug.Trace (trace)
import Data.IntMap.Lazy qualified as M
import Data.IntMap.Lazy (IntMap)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type T = String
type P = String

type Input = ([T], [P])

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

inputParser :: Parser Input
inputParser = do
    ts <- many (satisfy (`elem` ("bgruw" :: [Char]))) `sepBy` string ", "
    _ <- newline
    _ <- newline
    ps <- many (satisfy (/= '\n')) `sepBy` newline
    return (ts, ps)

----------- PART A&B -----------

testPattern :: [T] -> [T] -> P -> Int
testPattern _ _ [] = 1
testPattern _ [] _ = 0
testPattern allTs (t:ts) p = case stripPrefix t p of
    Nothing -> testPattern allTs ts p
    Just rest -> testPattern allTs allTs rest + testPattern allTs ts p

testPattern' :: [T] -> [T] -> P -> Bool
testPattern' _ _ [] = True
testPattern' _ [] _ = False
testPattern' allTs (t:ts) p = case stripPrefix t p of
    Nothing -> testPattern' allTs ts p
    Just rest -> testPattern' allTs allTs rest || testPattern' allTs ts p

type M = IntMap Int

count :: [T] -> P -> Int
count ts p = mFinal M.! (length p + 1)
    where
        mFinal = foldl foldTowel (M.empty :: M) [0..(length p)]
        foldTowel :: M -> Int -> M
        foldTowel m idx = foldl (\m' f -> f m') m $ map (\t -> if True then M.insertWith (+) (idx + length t) 1 else id) ts
            where
                x :: T -> (M -> M) -- TODO get pattern suffix to check
                x t = case stripPrefix t p of
                    Nothing -> id
                    _ -> M.insertWith (+) (idx + length t) 1

----------- PART A -------------

partA :: Input -> OutputA
partA (ts, ps) = length $ filter id $ map (testPattern' ts ts) ps

----------- PART B -------------

partB :: Input -> OutputB
partB (ts, ps) = sum $ map (testPattern ts ts) ps
