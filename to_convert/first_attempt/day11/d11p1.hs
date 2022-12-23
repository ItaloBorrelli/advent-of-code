{-# LANGUAGE ViewPatterns #-}
import Control.Monad ()
import Data.List ( delete, stripPrefix )

data Monkey = M { items :: [Int], operation :: (Int -> Int), test :: (Int -> Int)}

giveItem :: Monkey -> Int -> Monkey
giveItem m i = M (items m ++ [i]) (operation m) (test m)

throwItem :: Monkey -> Monkey
throwItem m = M ((tail . items) m) (operation m) (test m)

emptyMonkeyList :: Monkey -> Monkey
emptyMonkeyList m = M [] (operation m) (test m)

passTo :: Monkey -> Int -> Int
passTo m i = test m i

updateWorry :: Monkey -> Int -> Int
updateWorry m i = operation m i `div` 3

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = take i xs:(chunk i $ drop i xs)

readInt :: String -> Int
readInt = read

splitAtDelimiter :: String -> [Char] -> [String]
splitAtDelimiter _ [] = [[]]
splitAtDelimiter d (stripPrefix d -> Just rest) = []:splitAtDelimiter d rest
splitAtDelimiter d (x:xs) = (\y -> (x:head y):tail y) $ splitAtDelimiter d xs

selfOp :: Monad m => m (m a) -> m a
selfOp m = m >>= id

parseOperation :: [String] -> (Int -> Int)
parseOperation (_:op:val:_)
    | val == "old" = selfOp func
    | otherwise = func $ readInt val
    where func = if op == "+" then (+) else (*)

getItems :: String -> [Int]
getItems (stripPrefix "  Starting items: " -> Just inst) = map readInt $ splitAtDelimiter ", " inst

getOperation :: String -> (Int -> Int)
getOperation (stripPrefix "  Operation: new = " -> Just op) = parseOperation $ splitAtDelimiter " " op

getDivisor :: String -> Int
getDivisor (stripPrefix "  Test: divisible by " -> Just divisor) = readInt divisor

getTrueCondition :: String -> Int
getTrueCondition (stripPrefix "    If true: throw to monkey " -> Just m) = readInt m

getFalseCondition :: String -> Int
getFalseCondition (stripPrefix "    If false: throw to monkey " -> Just m) = readInt m

getTest :: [String] -> (Int -> Int)
getTest (divisor:true:false:_) = (\x ->
    if x `mod` (getDivisor divisor) == 0 then getTrueCondition true
    else getFalseCondition false)

readMonkey :: [String] -> Monkey
readMonkey details = M (getItems (details !! 1)) (getOperation (details !! 2)) (getTest (drop 3 details))

tossList :: Monkey -> [(Int, Int)]
tossList = (\m -> map ((\newI -> (test m newI, newI)) . (\i -> updateWorry m i)) $ items m)

replaceMonkey :: [a] -> Int -> a -> [a]
replaceMonkey ms n m = take n ms ++ [m] ++ drop (n+1) ms

monkeyToss :: [Monkey] -> Int -> [Monkey]
monkeyToss ms n =
    (\newL -> replaceMonkey newL n $ emptyMonkeyList (ms !! n))
    (foldl (\curr -> \(to, i) -> replaceMonkey curr to $ giveItem (curr !! to) i) ms $ tossList (ms !! n))

main :: IO ()
main = do
    contents <- readFile "./input"
    let monkeyList = map readMonkey $ chunk 7 $ lines contents
    let (_, y) =
            foldl (\(msi :: [Monkey], csi :: [Int]) -> \_ ->
                foldl (\(ms :: [Monkey], cs :: [Int]) -> \n ->
                    (monkeyToss ms n, replaceMonkey cs n $ (cs !! n) + (length $ items (ms !! n))))
                    (msi, csi)
                    [0..length monkeyList-1])
                (monkeyList, replicate (length monkeyList) 0)
                [1..20]
    let max1 = maximum y
    let max2 = maximum (delete max1 y)
    print $ max1 * max2