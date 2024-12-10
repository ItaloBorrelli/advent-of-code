module AOC.Y2024.Day09 (runDay) where

import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.List.Zipper (Zipper, cursor, fromList, fromListEnd, left, right, replace, start, insert, endp, beginp )
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
import Debug.Trace (trace)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

type BIn = (Int, Int)

type B = (Int, (Int, Int))

type Input = [BIn]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

readBlock :: Parser BIn
readBlock = ((,) . digitToInt <$> digit) <*> (digitToInt <$> (try digit <|> pure '0'))

inputParser :: Parser Input
inputParser = many readBlock <* eof

----------- PART A&B -----------

blockSum :: Int -> Int -> Int -> Int
blockSum fileId blockSize idx = trace (show (idx, fileId, blockSize)) sum $ map (fileId *) [idx .. (idx + blockSize - 1)]

----------- PART A -------------

zipChunks :: Int -> B -> Zipper B -> Zipper B -> B -> Int
zipChunks idx (id1, (f1, e1)) zf zb (id2, (f2, e2))
  -- after completing f1, knowing f1 and f2 are the same idx, we have reached the base case and use whatever is left in
  | id1 == id2 = blockSum id2 f2 idx
  -- f1 not added to chkSum -> calculate it, increment index & calculate the rest of the two files
  | f1 /= 0 = blockSum id1 f1 idx + zipChunks (idx + f1) (id1, (0, e1)) zf zb (id2, (f2, e2))
  -- no empty space remains in front -> use zf cursor value as next f1 & move along zf
  | e1 == 0 = zipChunks idx (cursor zf) (right zf) zb (id2, (f2, e2))
  -- f2 has been fully moved -> use zb cursor value as next f2 & move along zb
  | f2 == 0 = zipChunks idx (id1, (f1, e1)) zf (left zb) (cursor zb)
  -- f2 not added to chkSum -> calculate it at current index, increment index & calculate the rest of the two
  | otherwise = let a = min e1 f2 in blockSum id2 a idx + zipChunks (idx + a) (id1, (f1, e1 - a)) zf zb (id2, (f2 - a, e2))

partA :: Input -> OutputA
partA fs = (\(zf, zb) -> zipChunks 0 (cursor zf) (right zf) (left zb) (cursor zb)) $ (\fs' -> (fromList fs', left $ fromListEnd fs')) $ zip [0 ..] fs

----------- PART B -------------

q :: Int -> Int -> Int -> B -> Zipper B -> (Int, Int, B, Zipper B)
q exitOn idx total (id1, (f1, e1)) zl
  | exitOn == id1 = (idx, total, (id1, (f1, e1)), zl)
  | otherwise = q exitOn (idx + f1 + e1) (blockSum id1 f1 idx + total) (cursor zl) (right zl) 

chkSum :: Int -> (B, B) -> (Zipper B, Zipper B) -> Int
chkSum idx ((id1, (f1, e1)), (id2, (f2, e2))) (zl, zb)
  | endp zl || endp zb = 0
  | id1 == id2 && f1 + e1 == f2 + e2 = blockSum id2 f2 idx + chkSum (idx + f1 + e1) (cursor zl, cursor zb) (right zl, right zb)
  | otherwise = 
    let (id2', (f2', e2')) = cursor zb
        (idx', total, l', zl') = q id2' idx 0 (id1, (f1, e1)) zl
    in total + chkSum idx' (l', (id2', (f2', e2'))) (zl', right zb)
  -- | otherwise = trace "Case 2: " $ blockSum id1 f1 idx + chkSum (idx + f1 + e1) (cursor zl, (id2, (f2, e2))) (right zl, zb)

zipBlocks' :: Zipper B -> Zipper B -> (Zipper B, Zipper B)
zipBlocks' zf zb = if beginp zb then (start zf, start zb) else
  let (id1, (f1, e1)) = cursor zf
      (id2, (f2, e2)) = cursor zb
  in case () of
    _| id1 == id2 -> zipBlocks' (start zf) (left zb)
     | f2 == 0 -> zipBlocks' zf (left zb)
     | e1 == 0 -> zipBlocks' (right zf) zb
     | f2 <= e1 ->
      let zf0 = (id1, (f1, 0))
          zf1 = (id2, (f2, e1 - f2))
          zb1 = (id2, (0, f2 + e2))
          zf' = insert zf0 $ replace zf1 zf
          zb' = left $ replace zb1 zb
      in zipBlocks' (start zf') zb'
     | otherwise -> zipBlocks' (right zf) zb


partB :: Input -> OutputB
partB fs = (\(zf, zb) -> chkSum 0 (cursor zf, cursor zb) (right zf, right zb)) $ (\fs' -> zipBlocks' (fromList fs') (left $ fromListEnd fs')) $ zip [0 ..] fs
