splitAtDelimiter :: Eq a => a -> [a] -> [[a]]
splitAtDelimiter _ [] = [[]]
splitAtDelimiter d (x:xs)
    | x == d = []:rest
    | otherwise = (x:head rest):tail rest
    where rest = splitAtDelimiter d xs

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
    contents <- readFile "2022/01.txt"
    print $ maximum $ map (sum . map readInt)
            $ splitAtDelimiter ""
            $ lines contents
