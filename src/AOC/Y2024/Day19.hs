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

testPattern :: [T] -> [T] -> P -> Int
testPattern _ _ [] = 1
testPattern _ [] _ = 0
testPattern allTs (t : ts) p = case stripPrefix t p of
    Nothing -> testPattern allTs ts p
    Just rest -> testPattern allTs allTs rest + testPattern allTs ts p

testPattern' :: [T] -> [T] -> P -> Bool
testPattern' _ _ [] = True
testPattern' _ [] _ = False
testPattern' allTs (t : ts) p = case stripPrefix t p of
    Nothing -> testPattern' allTs ts p
    Just rest -> testPattern' allTs allTs rest || testPattern' allTs ts p

type M = IntMap Int

toChecks :: Int -> P -> [(Int, P)]
toChecks _ [] = []
toChecks idx p = (\x -> trace (show x) x) ((idx, p) : toChecks (idx + 1) (tail p))

count :: [T] -> P -> Int
count ts ps = fromMaybe 0 (mFinal M.!? length ps)
  where
    mFinal = foldl foldTowel (M.empty :: M) (toChecks 0 ps)
    foldTowel :: M -> (Int, P) -> M
    foldTowel m idxPattern = foldl (test idxPattern) m ts
      where
        test :: (Int, P) -> M -> T -> M
        test (idx, p) m' t =
            if length t > length p
                then m'
                else case stripPrefix t p of
                    Nothing -> m'
                    _ -> M.insertWith (+) (idx + length t) x m
          where
            x = if idx == 0 then 1 else fromMaybe 0 (m' M.!? idx)

----------- PART A -------------

partA :: Input -> OutputA
partA (ts, ps) = length $ filter id $ map (testPattern' ts ts) ps

----------- PART B -------------

partB :: Input -> OutputB
partB (ts, ps) = sum $ map (count ts) ps
