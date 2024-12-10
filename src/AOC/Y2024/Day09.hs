module AOC.Y2024.Day09 (runDay) where

import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.List.Zipper (Zipper, beginp, cursor, endp, fromList, fromListEnd, insert, left, replace, right, start, toList)
import Debug.Trace (trace)
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

data Block = Block
  { i :: Int,
    size :: Int,
    empty :: Int
  }
  deriving (Show, Eq)

type Input = [Block]

type OutputA = Int

type OutputB = Input

----------- PARSER -------------

readBlock :: Parser BIn
readBlock = ((,) . digitToInt <$> digit) <*> (digitToInt <$> (try digit <|> pure '0'))

block :: (Int, (Int, Int)) -> Block
block (i, (size, empty)) = Block {i = i, size = size, empty = empty}

inputParser :: Parser Input
inputParser = zipWith (curry block) [0 ..] <$> many readBlock <* eof

----------- PART A&B -----------

blockSum :: Int -> Int -> Int -> Int
blockSum fileId blockSize idx = sum $ map (fileId *) [idx .. (idx + blockSize - 1)]

-- (\x -> trace (show idx ++ "   id: " ++ show fileId ++ " size: " ++ show blockSize ++" total: "++ show x) x)

----------- PART A -------------

sizeToZero :: Block -> Block
sizeToZero Block {i, empty} = Block i 0 empty

reduceEmpty :: Block -> Int -> Block
reduceEmpty Block {i, size, empty} decrease = Block i size (empty - decrease)

reduceSize :: Block -> Int -> Block
reduceSize Block {i, size, empty} decrease = Block i (size - decrease) empty

moveRight :: Zipper Block -> (Block, Zipper Block)
moveRight z = (cursor z, right z)

moveLeft :: Zipper Block -> (Block, Zipper Block)
moveLeft z = (cursor z, left z)

-- | zipChunks calculates the sum of blocks by zipping two lists of blocks where chunks of blocks can be split up.
-- | Values in zippers are never modified, we simply calculate what can be moved from the right to the left and calculate what would move.
-- |
-- | Parameters:
-- | idx - The current index in the block list.
-- | (x, zx) - A tuple containing the current front block and the zipper for the front blocks.
-- | (y, zy) - A tuple containing the current back block and the zipper for the back blocks.
zipChunks :: Int -> (Block, Zipper Block) -> (Block, Zipper Block) -> Int
zipChunks idx (x, zx) (y, zy)
  -- base case (zippers are at the same index): complete evaluation of x
  | i x == i y = blockSum (i y) (size y) idx
  -- x not calculated: complete evaluation of x -> increment index -> re-evaluate new x & y
  | size x /= 0 = blockSum (i x) (size x) idx + zipChunks (idx + size x) (sizeToZero x, zx) (y, zy)
  -- no empty space at x: move right on zx
  | empty x == 0 = zipChunks idx (moveRight zx) (y, zy)
  -- no chunks left in y: move left on zy
  | size y == 0 = zipChunks idx (x, zx) (moveLeft zy)
  -- space at x and chunks in y: evaluate what can be moved of y -> increment index -> reduce empty space in x -> reduce size of y -> re-evaluate new x & y
  | otherwise = let available = min (empty x) (size y) in blockSum (i y) available idx + zipChunks (idx + available) (reduceEmpty x available, zx) (reduceSize y available, zy)

partA :: Input -> OutputA
partA = (\(zf, zb) -> zipChunks 0 (cursor zf, right zf) (cursor zb, left zb)) . (\fs' -> (fromList fs', left $ fromListEnd fs'))

----------- PART B -------------

q :: Int -> Int -> Int -> B -> Zipper B -> (Int, Int, B, Zipper B)
q exitOn idx total (id1, (f1, e1)) zl
  | exitOn == id1 = (idx, total, cursor $ left zl, left $ left zl)
  | otherwise = q exitOn (idx + f1 + e1) (total + blockSum id1 f1 idx) (cursor zl) (right zl)

-- trace ("Case B: " ++ show (exitOn, id1, f1, idx)) $
chkSum :: Int -> (B, B) -> (Zipper B, Zipper B) -> Int
chkSum idx ((id1, (f1, e1)), (id2, (f2, e2))) (zl, zb)
  | endp zl || endp zb = 0
  | id1 == id2 && f1 + e1 == f2 + e2 = blockSum id2 f2 idx + chkSum (idx + f1 + e1) (cursor zl, cursor zb) (right zl, right zb)
  | otherwise =
      let (id2', (f2', e2')) = cursor zb
          (idx', total, l', zl') = q id2' idx 0 (id1, (f1, e1)) zl
       in total + chkSum idx' (l', (id2', (f2', e2'))) (zl', right zb)

-- \| otherwise = trace "Case 2: " $ blockSum id1 f1 idx + chkSum (idx + f1 + e1) (cursor zl, (id2, (f2, e2))) (right zl, zb)

zipBlocks' :: Zipper B -> Zipper B -> (Zipper B, Zipper B)
zipBlocks' zf zb =
  if beginp zb
    then (start zf, start zb)
    else
      let (id1, (f1, e1)) = cursor zf
          (id2, (f2, e2)) = cursor zb
       in case () of
            _
              | id1 == id2 -> zipBlocks' (start zf) (left zb)
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
-- partB fs = (\(zf, zb) -> trace (show $ toList zf) $ trace (show $ toList zb) $ chkSum 0 (cursor zf, cursor zb) (right zf, right zb)) $ (\fs' -> zipBlocks' (fromList fs') (left $ fromListEnd fs')) $ zip [0 ..] fs
partB fs = fs
