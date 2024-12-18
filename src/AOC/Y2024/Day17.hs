module AOC.Y2024.Day17 (runDay) where

import Data.Bits (Bits ((.&.), (.|.)), shiftL, shiftR, xor)
import Data.List (intercalate)
import Data.List.Extra ((!?))
import Data.Maybe (isNothing)
import Data.Set (Set, empty, insert)
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, sepBy, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec (many1)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type P = [Int]

type Input = (R, P)

type OutputA = String

type OutputB = Maybe Int

----------- PARSER -------------

inputParser :: Parser Input
inputParser = do
    _ <- string "Register A: "
    a <- read <$> many1 digit
    _ <- newline
    _ <- string "Register B: "
    b <- read <$> many1 digit
    _ <- newline
    _ <- string "Register C: "
    c <- read <$> many1 digit
    _ <- newline
    _ <- newline
    _ <- string "Program: "
    p <- map read <$> many1 digit `sepBy` char ','
    return (R a b c, p)

----------- PART A&B -----------

type Opcode = Int

type Operand = Int

type Ops = (Opcode, Operand)

data R = R {a :: Int, b :: Int, c :: Int} deriving (Eq, Show, Ord)

type Ptr = Int

type In = (R, Ptr)

type Out = (R, Ptr)

getCombo :: Operand -> R -> Int
getCombo 4 (R a _ _) = a
getCombo 5 (R _ b _) = b
getCombo 6 (R _ _ c) = c
getCombo o _ = o -- 7 or higher should never exist as an operand

runProgram :: Ops -> In -> Either Out (Out, Int)
runProgram (op, l) (r@(R a b c), p) = case op of
    0 -> Left adv
    1 -> Left bxl
    2 -> Left bst
    3 -> Left jnz
    4 -> Left bxc
    5 -> Right out
    6 -> Left bdv
    _ -> Left cdv -- case 7
  where
    combo = getCombo l r
    p' = p + 2
    q = a `shiftR` combo
    adv, bxl, bst, jnz, bxc, bdv, cdv :: Out
    adv = (R q b c, p')
    bxl = (R a (b `xor` l) c, p')
    bst = (R a (combo .&. 7) c, p')
    jnz = case a of
        0 -> (r, p')
        _ -> (r, l)
    bxc = (R a (b `xor` c) c, p')
    bdv = (R a q c, p')
    cdv = (R a b q, p')
    out :: (Out, Int)
    out = ((r, p'), combo .&. 7)

----------- PART A -------------

partA :: Input -> OutputA
partA (initReg, prog) = intercalate "," $ map show $ run (initReg, 0)
  where
    run :: In -> [Int]
    run in'@(_, p) = case prog !? p of
        Nothing -> []
        Just op -> case result of
            Left next -> run next
            Right (next, output) -> output : run next
          where
            l = prog !! (p + 1)
            result = runProgram (op, l) in'

----------- PART B -------------

chunks :: [Int]
chunks = [0 .. 7]

combineA :: Int -> Int -> Int
combineA a x = (a `shiftL` 3) .|. x

isValid :: Int -> Int -> Bool
isValid a outExpected = outActual == outExpected
  where
    b' = (a .&. 7) `xor` 1
    outActual = ((b' `xor` 5) `xor` (a `shiftR` b')) .&. 7

formulateA :: [Int] -> Int -> Maybe Int
formulateA [] a = Just a
formulateA prog a =
    let
        testValues = map (combineA a) chunks
     in
        check testValues (head prog)
  where
    check :: [Int] -> Int -> Maybe Int
    check [] _ = Nothing
    check (t : ts) out =
        if isValid t out
            then case formulateA (tail prog) t of
                Nothing -> check ts out
                x -> x
            else check ts out

partB :: Input -> OutputB
partB (_, prog) = formulateA (reverse prog) 0
