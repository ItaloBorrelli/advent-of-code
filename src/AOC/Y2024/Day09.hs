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
import Data.Tuple.Extra (both)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

----------- TYPES --------------

data Block = Block
  { i :: Int,
    size :: Int,
    empty :: Int
  }
  deriving (Show, Eq)

type Input = [Block]

type OutputA = Int

type OutputB = Int

----------- PARSER -------------

readBlock :: Parser (Int, Int)
readBlock = ((,) . digitToInt <$> digit) <*> (digitToInt <$> (try digit <|> pure '0'))

block :: (Int, (Int, Int)) -> Block
block (i, (size, empty)) = Block {i = i, size = size, empty = empty}

inputParser :: Parser Input
inputParser = zipWith (curry block) [0 ..] <$> many readBlock <* eof

----------- PART A&B -----------

blockSum' :: Int -> Block -> Int
blockSum' idx Block {i, size} = sum $ map (i *) [idx .. (idx + size - 1)]

moveRight :: Zipper Block -> (Block, Zipper Block)
moveRight z = (cursor z, right z)

----------- PART A -------------

sizeToZero :: Block -> Block
sizeToZero Block {i, empty} = Block i 0 empty

reduceEmpty :: Block -> Int -> Block
reduceEmpty Block {i, size, empty} decrease = Block i size (empty - decrease)

reduceSize :: Block -> Int -> Block
reduceSize Block {i, size, empty} decrease = Block i (size - decrease) empty


moveLeft :: Zipper Block -> (Block, Zipper Block)
moveLeft z = (cursor z, left z)

-- | zipChunks calculates the sum of blocks by zipping two lists of blocks where chunks of blocks can be split up.
-- | Values in zippers are never modified, we simply calculate what can be moved from the right to the left and calculate what would move.
-- |
-- | Parameters:
-- | idx - The current index in the block list.
-- | (x, xz) - A tuple containing the current front block and the zipper for the front blocks.
-- | (y, yz) - A tuple containing the current back block and the zipper for the back blocks.
zipChunks :: Int -> (Block, Zipper Block) -> (Block, Zipper Block) -> Int
zipChunks idx (x, xz) (y, yz)
  -- base case (zippers are at the same index): complete evaluation of x
  | i x == i y = blockSum' idx y
  -- x not calculated: complete evaluation of x -> increment index -> re-evaluate new x & y
  | size x /= 0 = blockSum' idx x + zipChunks (idx + size x) (sizeToZero x, xz) (y, yz)
  -- no empty space at x: move right on xz
  | empty x == 0 = zipChunks idx (moveRight xz) (y, yz)
  -- no chunks left in y: move left on yz
  | size y == 0 = zipChunks idx (x, xz) (moveLeft yz)
  -- space at x and chunks in y: evaluate what can be moved of y -> increment index -> reduce empty space in x -> reduce size of y -> re-evaluate new x & y
  | otherwise = let available = min (empty x) (size y) in blockSum' idx (Block (i y) available 0) + zipChunks (idx + available) (reduceEmpty x available, xz) (reduceSize y available, yz)

partA :: Input -> OutputA
partA = (\(zf, zb) -> zipChunks 0 (cursor zf, right zf) (cursor zb, left zb)) . (\fs' -> (fromList fs', left $ fromListEnd fs'))

----------- PART B -------------

emptyToZero :: Block -> Block
emptyToZero Block {i, size} = Block i size 0

fullEmpty :: Block -> Block
fullEmpty Block {i, size, empty} = Block i 0 (size + empty)

moveToEmpty :: Int -> Block -> Block
moveToEmpty emptyX Block {i, size} = Block i size (emptyX - size)

zipBlocks :: Zipper Block -> Zipper Block -> (Zipper Block, Zipper Block)
zipBlocks xz yz
  | beginp yz = (start xz, start yz)
  | i x == i y = zipBlocks (start xz) (left yz)
  | size y == 0 = zipBlocks xz (left yz)
  | empty x == 0 = zipBlocks (right xz) yz
  | size y <= empty x = zipBlocks (start $ insert (emptyToZero x) $ replace (moveToEmpty (empty x) y) xz) (left $ replace (fullEmpty y) yz)
  | otherwise = zipBlocks (right xz) yz
  where (x, y) = both cursor (xz, yz)


blockSize :: Block -> Int
blockSize Block {size, empty} = size + empty

checkSum :: Int -> (Block, Zipper Block) -> (Block, Zipper Block) -> [Int] -> Int
checkSum idx (x, xz) (y, yz) counted
  -- at end of xz (base case)
  | endp xz = 0
  | i x `elem` counted = checkSum (idx + blockSize x) (moveRight xz) (y, yz) counted
  | i x /= i y = blockSum' idx x + checkSum (idx + blockSize x) (moveRight xz) (y, yz) (i x:counted)
  | blockSize x == blockSize y = blockSum' idx y + checkSum (idx + blockSize y) (moveRight xz) (moveRight yz) counted
  | otherwise = checkSum idx (x, xz) (moveRight yz) counted

partB :: Input -> OutputB
partB = (\(xz, yz) -> checkSum 0 (cursor xz, right xz) (cursor yz, right yz) []) . (\fs' -> zipBlocks (fromList fs') (left $ fromListEnd fs'))
