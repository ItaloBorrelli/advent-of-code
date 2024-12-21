module AOC.Y2024.Day19 (runDay) where

import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as M
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, many, newline, sepBy, string)
import Text.Parsec.Char (anyChar, satisfy)
import Text.Parsec.Text (Parser)

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

testPattern' :: [T] -> [T] -> P -> Bool
testPattern' _ _ [] = True
testPattern' _ [] _ = False
testPattern' allTs (t : ts) p = case stripPrefix t p of
    Nothing -> testPattern' allTs ts p
    Just rest -> testPattern' allTs allTs rest || testPattern' allTs ts p

type M = IntMap Int

slice :: [a] -> Int -> Int -> [a]
slice ls i j = take j $ drop i ls

count :: [T] -> P -> Int
count ts ps = fromMaybe 0 (mFinal M.!? (length ps))
  where
    mFinal = foldr foldTowel (M.singleton 0 1) [1 .. length ps + 1]
    foldTowel :: Int -> M -> M
    foldTowel idx m = foldl (testPattern idx) m ts
      where
        testPattern :: Int -> M -> T -> M
        testPattern i m' t =
            if length t <= i && slice ps (i - length t) i == t
                then trace ( "in at: " ++ show (ps, t, i) )M.insertWith (+) (i) x m
                else m'
          where
            x = fromMaybe 0 (trace (show (i - length t)) m' M.!?  (i - length t))

----------- PART A -------------

partA :: Input -> OutputA
partA (ts, ps) = length $ filter id $ map (testPattern' ts ts) ps

----------- PART B -------------

partB :: Input -> OutputB
partB (ts, ps) = sum $ map (count ts) ps
