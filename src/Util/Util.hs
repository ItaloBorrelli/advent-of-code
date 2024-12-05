module Util.Util
  ( freq,
    mapFromNestedLists,
    chunksOf,
    chunksByPredicate,
    traceShowIdWithContext,
    (!!?),
    mapBoundingBox,
    both,
    safeTake,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Debug.Trace (trace)

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
    | n <= 0 = error "Cannot split into chunks of negative length."
    | null ls = []
    | length ls < n = [ls]
    | otherwise = take n ls : chunksOf n (drop n ls)

-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
    | null ls = []
    | otherwise =
        let
            (prefix, rest) = span p ls
         in
            if null prefix
                then chunksByPredicate p $ dropWhile (not . p) rest
                else prefix : chunksByPredicate p (dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
    if
        | index < 0 -> Nothing
        | index >= length list -> Nothing
        | otherwise -> Just $ list !! index

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
mapBoundingBox :: Map (Int, Int) a -> (Int, Int, Int, Int)
mapBoundingBox m =
    (,,,)
        (minimum . fmap fst . Map.keys $ m)
        (maximum . fmap fst . Map.keys $ m)
        (minimum . fmap snd . Map.keys $ m)
        (maximum . fmap snd . Map.keys $ m)

-- Applies a function to both the first and second part of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- Takes as many as it can, returning a list of the given number or the length of the list, whichever is lower.
safeTake :: Int -> [a] -> [a]
safeTake 0 _ = []
safeTake _ [] = []
safeTake n (x : xs) = x : safeTake (n - 1) xs
