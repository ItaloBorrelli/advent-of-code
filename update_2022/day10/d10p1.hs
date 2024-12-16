{-# LANGUAGE ViewPatterns #-}
import Data.List ( stripPrefix )

type Clock = Int
type Register = Int

updateCycle :: (Clock, Register, [Int], Int) -> Int -> (Clock, Register, [Int], Int)
updateCycle (c, r, sig, strength) v
    | c `elem` sig = (newC, newR, tail sig, head sig * r + strength)
    | otherwise = (newC, newR, sig, strength)
    where (newC, newR) = (c+1, r+v)

parseInst :: String -> [Int]
parseInst (stripPrefix "noop" -> Just end) = [0]
parseInst (stripPrefix "addx " -> Just add) = [0, read add :: Int]

main :: IO ()
main = do
    contents <- readFile "./input"
    let instList = foldr (++) [] $ map parseInst $ lines contents
    let sigList = map (\x -> x*40-20) [1..6]
    let (_, _, _, str) = foldl updateCycle (1, 1, sigList, 0) instList
    print str