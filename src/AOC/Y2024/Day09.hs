module AOC.Y2024.Day09 (runDay) where

import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.List.Zipper (Zipper, cursor, fromList, fromListEnd, left, right)
import Data.Void (Void)
import Program.RunDay qualified as R
  ( Day,
    runDay,
  )
import Text.Parsec
  ( digit,
    eof,
    many,
    try,
  )
import Text.Parsec.Text (Parser)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type BIn = (Int, Int)

type B = (Int, (Int, Int))

type Input = [BIn]

type OutputA = Int

type OutputB = Void

----------- PARSER -------------

readBlock :: Parser BIn
readBlock = ((,) . digitToInt <$> digit) <*> (digitToInt <$> (try digit <|> pure '0'))

inputParser :: Parser Input
inputParser = many readBlock <* eof

----------- PART A&B -----------

blockSum :: Int -> Int -> Int -> Int
blockSum fileId blockSize idx = sum $ map (fileId *) [idx .. (idx + blockSize - 1)]

zipBlocks :: Int -> B -> Zipper B -> Zipper B -> B -> Int
zipBlocks idx (id1, (f1, e1)) zf zb (id2, (f2, e2))
  -- after completing f1, knowing f1 and f2 are the same idx, we have reached the base case and use whatever is left in
  | id1 == id2 = blockSum id2 f2 idx
  -- f1 not added to chkSum -> calculate it, increment index & calculate the rest of the two files
  | f1 /= 0 = blockSum id1 f1 idx + zipBlocks (idx + f1) (id1, (0, e1)) zf zb (id2, (f2, e2))
  -- no empty space remains in front -> use zf cursor value as next f1 & move along zf
  | e1 == 0 = zipBlocks idx (cursor zf) (right zf) zb (id2, (f2, e2))
  -- f2 has been fully moved -> use zb cursor value as next f2 & move along zb
  | f2 == 0 = zipBlocks idx (id1, (f1, e1)) zf (left zb) (cursor zb)
  -- f2 not added to chkSum -> calculate it at current index, increment index & calculate the rest of the two
  | otherwise = let a = min e1 f2 in blockSum id2 a idx + zipBlocks (idx + a) (id1, (f1, e1 - a)) zf zb (id2, (f2 - a, e2))

----------- PART A -------------

partA :: Input -> OutputA
partA fs = (\(zf, zb) -> zipBlocks 0 (cursor zf) (right zf) (left zb) (cursor zb)) $ (\fs' -> (fromList fs', left $ fromListEnd fs')) $ zip [0 ..] fs

----------- PART B -------------

partB :: Input -> OutputB
partB = error "Not implemented yet!"
