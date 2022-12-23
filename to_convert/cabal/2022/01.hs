import           Data.List  (sort)
import           Data.Maybe (fromJust)
import           System.IO  ()
import           Text.Read  (readMaybe)

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = [[]]
split' def (x:xs)
    | x == def = []:rest
    | otherwise = (x:head rest):tail rest
    where rest = split' def xs

main :: IO ()
main = do
    contents <- readFile "2022/01.txt"
    print $ maximum
        $ map (sum . map fromJust)
        $ split' Nothing
        $ map (readMaybe :: String -> Maybe Int)
        $ lines contents
    print $ sum
        $ take 3
        $ reverse
        $ sort
        $ map (sum . map fromJust)
        $ split' Nothing
        $ map (readMaybe :: String -> Maybe Int)
        $ lines contents
