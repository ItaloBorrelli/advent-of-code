{-# LANGUAGE ViewPatterns #-}
import Data.List ( stripPrefix )
import Data.Sequence ( chunksOf )

type Clock = Int
type Register = Int

updateCycle :: (Clock, Register, [Char]) -> Int -> (Clock, Register, [Char])
updateCycle (c, r, screen) v
    | drawn == r || drawn == r-1 || drawn == r+1 = (newC, newR, take (c-1) screen ++ '#':(tail . drop (c-1)) screen)
    | otherwise = (newC, newR, screen)
    where (newC, newR, drawn) = (c+1, r+v, (c-1) `mod` 40)

parseInst :: String -> [Int]
parseInst (stripPrefix "noop" -> Just end) = [0]
parseInst (stripPrefix "addx " -> Just add) = [0, read add :: Int]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = take i xs:(chunk i $ drop i xs)

main :: IO ()
main = do
    contents <- readFile "./input"
    let instList = foldr (++) [] $ map parseInst $ lines contents
    let (_, _, screen) = foldl updateCycle (1, 1, replicate 240 '.') instList
    putStr $ unlines $ chunk 40 screen