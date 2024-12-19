module AOC.Y2024.Day17 (runDay) where

import Data.Bits (Bits ((.&.), (.|.)), shiftL, shiftR, xor)
import Data.List (intercalate)
import Data.List.Extra ((!?))
import Program.RunDay qualified as R (Day, runDay)
import Text.Parsec (char, digit, sepBy)
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec (many1)
import Util.Util (skipNonDigits, unsignedInt)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type P = [Int]

type Input = (R, P)

type OutputA = String

type OutputB = Int

----------- PARSER -------------

parseReg :: Parser Int
parseReg = skipNonDigits *> unsignedInt

inputParser :: Parser Input
inputParser =
    (\a b c p -> (R a b c, p))
        <$> parseReg
        <*> parseReg
        <*> parseReg
        <*> ( skipNonDigits
                *> (map read <$> many1 digit `sepBy` char ',')
            )

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

isValid :: [Ops] -> Int -> Int -> Bool
isValid prog a outExpected = outActual == Just outExpected
  where
    (_, _, outActual) =
        foldl
            ( \(r, ptr, output) op -> case runProgram op (r, ptr) of
                Left (r', ptr') -> (r', ptr', output)
                Right ((r', ptr'), out) -> (r', ptr', Just out)
            )
            (R a 0 0, 0, Nothing)
            prog

formulateA :: (Int -> Int -> Bool) -> P -> Int -> Maybe Int
formulateA _ [] a = Just a
formulateA loopFunc gorp a =
    let
        testValues = map (combineA a) chunks
     in
        check testValues (head gorp)
  where
    check :: [Int] -> Int -> Maybe Int
    check [] _ = Nothing
    check (t : ts) out =
        if loopFunc t out
            then case formulateA loopFunc (tail gorp) t of
                Nothing -> check ts out
                x -> x
            else check ts out

genOps :: P -> [Ops]
genOps [3, 0] = []
genOps (x : y : rest) = (x, y) : genOps rest
genOps _ = fail "Bad program: Program must end with 30"

partB :: Input -> OutputB
partB (_, prog) = case formulateA (isValid (genOps prog)) (reverse prog) 0 of
    Nothing -> error "Failed: Program assumes one print per loop and A right shifted by 3 every loop (i.e. 03 in program)."
    Just a -> a
