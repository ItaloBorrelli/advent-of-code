import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.List (sortBy)
import Data.Ord (compare)
import Text.ParserCombinators.ReadP (char, readP_to_S, readS_to_P, sepBy, ReadP)

data P = Num Int | List [P] deriving (Eq, Ord, Show)
t :: ReadP P
t = List <$ char '[' <*> t `sepBy` char ',' <* char ']' <|>
    Num <$> readS_to_P reads

parse :: ReadP a -> String -> a
parse r s = fst $ (readP_to_S r s) !! 0

chunk :: Int -> Int -> [a] -> [[a]]
chunk _ _ [] = []
chunk i j xs = take i xs:(chunk i j $ drop j xs)

tuplify :: [a] -> (a, a)
tuplify (x:y:_) = (x, y)

compareP :: P -> P -> Ordering
compareP (Num x) (Num y) = compare x y
compareP (List xs) (List ys) = comparePs xs ys
compareP (Num x ) (List ys) = comparePs [Num x] ys
compareP (List xs) (Num y ) = comparePs xs [Num y]

comparePs :: [P] -> [P] -> Ordering
comparePs (x:xs) (y:ys) = compareP x y <> comparePs xs ys
comparePs [] [] = EQ
comparePs [] _ = LT
comparePs _ _ = GT

main :: IO ()
main = do
    contents <- readFile "./input"
    print "part1"
    print $ sum
        $ map (\(idx, _) -> idx)
        $ filter (\(_, ord) -> ord == LT)
        $ zip [1..] $ map (uncurry compareP . tuplify . map (parse t)) $ chunk 2 3 $ lines contents
    print "part2"
    let new = map (parse t) ["[[2]]", "[[6]]"]
    print $ product [idx | (idx, x) <- zip [1..]
        $ sortBy compareP
        $ (++) new
        $ map (parse t)
        $ foldr (++) [] $ chunk 2 3 $ lines contents, x `elem` new]
