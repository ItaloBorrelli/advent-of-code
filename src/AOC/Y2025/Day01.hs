module AOC.Y2025.Day01 (runDay) where

import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec
import Text.Parsec.Text

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type Move = (Dir, Int)

type Input = [Move]

data Dir = L | R deriving (Show)

type OutputA = Int

type OutputB = Int

----------- PARSER -------------
readLine :: Parser (Dir, Int)
readLine = (,) <$> ((L <$ char 'L') <|> (R <$ char 'R')) <*> (read <$> many1 digit)

inputParser :: Parser Input
inputParser = readLine `sepEndBy` newline <* eof

----------- PART A&B -----------

----------- PART A -------------

doMove :: (Int, Int) -> Move -> (Int, Int)
doMove (x, pos) (L, n) =
    let
        new_pos = (pos - n) `mod` 100
     in
        if new_pos == 0 then (x + 1, new_pos) else (x, new_pos)
doMove (x, pos) (R, n) =
    let
        new_pos = (pos + n) `mod` 100
     in
        if new_pos == 0 then (x + 1, new_pos) else (x, new_pos)

partA :: Input -> OutputA
partA = fst . foldl doMove (0, 50)

----------- PART B -------------

doMove2 :: (Int, Int) -> Move -> (Int, Int)
doMove2 (x, pos) (L, n)
    | n == 0 = (x, pos)
    | otherwise =
        let
            new_pos = (pos - n)
            y = (pos - n) `mod` 100
         in
            if pos == 0
                then (x + ((((new_pos - 1) `div` 100) * (-1)) - 1), y)
                else if (pos - n) <= 0 then doMove2 (x + 1, 0) (L, n - pos) else (x, pos - n)
doMove2 (x, pos) (R, n) =
    let
        new_pos = (pos + n)
        y = (pos + n) `mod` 100
     in
        if new_pos >= 100 then (x + (new_pos `div` 100), y) else (x, y)

partB :: Input -> OutputB
partB = fst . foldl doMove2 (0, 50)
