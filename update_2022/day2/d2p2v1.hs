import System.Environment ( getArgs )
import System.IO ()

roundEndPoints :: Char -> Int
roundEndPoints x
    | x == 'X' = 0
    | x == 'Y' = 3
    | otherwise = 6

opponentChoice :: Char -> Int
opponentChoice x
    | x == 'A' = 1
    | x == 'B' = 2
    | otherwise = 3

choicePoints :: Char -> Char -> Int
choicePoints i j
    | j == 'X' = (d - 2) `mod` 3 + 1
    | j == 'Z' = d `mod` 3 + 1
    | otherwise = d
    where d = opponentChoice i

getPoints :: [Char] -> Int
getPoints [i, _, j] = roundEndPoints j + choicePoints i j
getPoints _ = 0

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    print $ sum $ map getPoints (lines contents)