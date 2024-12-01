module AOC.Y2024.Day01 (runDay) where

import           Data.Void              (Void)
import           Debug.Trace            (trace)
import qualified Program.RunDay         as R (Day, runDay)
import           Text.Parsec            (digit, many1, space)
import           Text.Parsec.Char       (newline)
import           Text.Parsec.Combinator (sepEndBy)
import           Text.Parsec.Text       (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
splitLine :: Parser (Int', Int')
splitLine = do
  num1 <- many1 digit
  _ <- many1 space
  num2 <- many1 digit
  return (Finite $ read num1, Finite $ read num2)

inputParser :: Parser Input
inputParser = splitLine `sepEndBy` newline

------------ TYPES -------------
type Input = [(Int', Int')]

type OutputA = Int'

type OutputB = Void

data Int' = Finite Int | Inf
  deriving (Eq)

instance Num Int' where
    (+) :: Int' -> Int' -> Int'
    (Finite x) + (Finite y) = Finite (x + y)
    _ + _                   = Inf

    (*) :: Int' -> Int' -> Int'
    (Finite x) * (Finite y) = Finite (x * y)
    _ * _                   = Inf

    abs :: Int' -> Int'
    abs (Finite x) = Finite (abs x)
    abs Inf        = Inf

    signum :: Int' -> Int'
    signum (Finite x) = Finite (signum x)
    signum Inf        = Finite 1

    fromInteger :: Integer -> Int'
    fromInteger x = Finite (fromInteger x)

    negate :: Int' -> Int'
    negate (Finite x) = Finite (negate x)
    negate Inf        = Inf

instance Show Int' where
    show :: Int' -> String
    show (Finite x) = show x
    show Inf        = "Inf"

instance Ord Int' where
    compare :: Int' -> Int' -> Ordering
    compare Inf _                 = GT
    compare _ Inf                 = LT
    compare (Finite x) (Finite y) = compare x y

------------ UTIL --------------


-- Go forward through list and find the minimum element
-- Then go back through the list and replace all instances of the minimum element with Infinity
-- Returns the list with the minimum element replaced with Infinity and the positions of the minimum elements
findAndReplaceMins :: [(Int', Int')] -> Int -> (Int', Int') -> (Int', [(Int', Int')], ([Int'], [Int']), (Int', Int'))
findAndReplaceMins [] _ m = (Inf, [], ([Inf], [Inf]), m)
findAndReplaceMins ((x1, x2):xs) i (m1, m2) =
    let (min_dist, xs', (xpositions, ypositions), (m1', m2')) = findAndReplaceMins xs (i + 1) (min m1 x1, min m2 x2)
    in
        let ((x1', xpositions'), (x2', ypositions')) =
                (if x1 == m1' then (Inf, Finite i:xpositions) else (x1, xpositions)
                ,if x2 == m2' then (Inf, Finite i:ypositions) else (x2, ypositions)
                )
        in (min min_dist $ abs (head xpositions' - head ypositions'), (x1', x2'):xs', (xpositions', ypositions'), (m1', m2'))

------------ PART A ------------
partA :: Input -> OutputA
partA = go 0
    where
        go dist_sums xs =
            let (min_dist, xs', _, (m1, m2)) = findAndReplaceMins xs 1 (Inf, Inf)
            in if m1 == Inf || m2 == Inf
                --  then dist_sums
                --  else trace (show (min_dist, xs', (m1, m2))) go (min_dist + dist_sums) xs'
                 then dist_sums
                 else trace (show min_dist) go (min_dist+dist_sums) xs'

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
