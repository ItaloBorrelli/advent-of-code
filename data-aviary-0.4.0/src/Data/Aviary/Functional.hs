{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.Functional
-- Copyright   :  (c) Stephen Peter Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Functor, Applicative, Monad operations /specialized/ to
-- the functional type.
--
-- This catalogue is for reference and is not intended for use.
--
-----------------------------------------------------------------------------

module Data.Aviary.Functional
  (
  -- * Functor
    fmap

  -- * Applicative
  , (<$>)
  , (<$)

  , pure
  , (<*>)
  , (*>)
  , (<*)

  , (<**>)
  , liftA, liftA2, liftA3

  -- * Category
  , id
  , (.)
  , (<<<)
  , (>>>)

  -- * Monad
  , (>>=)
  , (>>)
  , return
  , fail
  , mapM, mapM_
  , forM, forM_
  , sequence, sequence_
  , (=<<)
  , (>=>), (<=<)
  , forever
  , join
  , filterM
  , mapAndUnzipM
  , zipWithM, zipWithM_
  , foldM, foldM_
  , replicateM, replicateM_
  , when
  , unless
  , liftM, liftM2, liftM3, liftM4, liftM5
  , ap

  -- * Arrow
  , arr
  , first
  , second
  , (***)
  , (&&&)

  , returnA
  , (^>>), (>>^)
  , (<<^), (^<<)

  , left, right
  , (+++), (|||)
  , app, leftApp
  , loop

  -- * Comonad
  , extract
  , duplicate
  , extend
  , liftW
  , (=>>), (.>>)
  , liftCtx
  , mapW
  , parallelW
  , unfoldW
  , sequenceW

  ) where

import qualified Control.Applicative as App
import qualified Control.Arrow       as Arr
import qualified Control.Category    as Cat
import qualified Control.Monad       as Mon

import           Data.Monoid         (Monoid (..))
import           Prelude             (Bool, Either, Int, String, fst, head, snd,
                                      tail)

--------------------------------------------------------------------------------
-- Functor

-- ((->) r) replaces the type variable f


-- | 'fmap' for the function instance of Functor is /compose/
-- ('.') which in turn is 'Data.Aviary.Birds.bluebird'.
--
fmap        :: (a -> b) -> (r -> a) -> (r -> b)
fmap        = Mon.fmap


--------------------------------------------------------------------------------
-- Control.Applicative

-- ((->) r) replaces the type variable f

-- | The Applicative combinator ('<$>') is a synonym for 'fmap',
-- so for the function instance of of Applicative it is
-- /compose/ ('.') which is 'Data.Aviary.Birds.bluebird'.
--
(<$>)       :: (a -> b) -> (r -> a) -> (r -> b)
(<$>)       = Mon.fmap

-- | Applicative ('<$').
--
(<$)        :: a -> (r -> b) -> (r -> a)
(<$)        = (App.<$)

-- Applicative class


-- | The function instance of Applicative 'pure' is
-- 'const' which is 'Data.Aviary.Birds.kestrel'.
--
pure        :: a -> (r -> a)
pure        = App.pure


-- | The combinator ('<*>') for the function instance of
-- Applicative is the S combinator aka
-- 'Data.Aviary.Birds.starling'.
--
(<*>)       :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*>)       = (App.<*>)

(*>)        :: (r -> a) -> (r -> b) -> (r -> b)
(*>)        = (App.*>)

(<*)        :: (r -> a) -> (r -> b) -> (r -> a)
(<*)        = (App.<*)

-- No function instance of Alternative.



(<**>)      :: (r -> a) -> (r -> a -> b) -> (r -> b)
(<**>)      = (App.<**>)

-- | The Applicative function 'liftA' is a synonym for 'fmap',
-- so for the function instance of of Applicative it is
-- /compose/ ('.') which is 'Data.Aviary.Birds.bluebird'.
--
liftA       :: (a -> b) -> (r -> a) -> (r -> b)
liftA       = App.liftA


-- | 'liftA2' for the function instance of Applicative is the
-- 'Data.Aviary.Birds.phoenix' combinator, also called big Phi
-- and /starling-prime/.
--
liftA2      :: (a -> b -> c) -> (r -> a) -> (r -> b) -> (r -> c)
liftA2      = App.liftA2

liftA3      :: (a -> b -> c -> d) -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d)
liftA3      = App.liftA3

-- No optional (due to no Alternative instance)



--------------------------------------------------------------------------------
-- Control.Category

-- (->) replaces the type variable cat


-- | For the function instance of Category @id@ is just the
-- identity function (Haskell\'s 'id').
--
id          :: a -> a
id          = Cat.id

-- | For the function instance of Category composition is just
-- regular function composition aka 'Data.Aviary.Birds.bluebird'.
--
(.)         ::  (b -> c) -> (a -> b) -> (a -> c)
(.)         = (Cat..)

-- | For the function instance of Category right-to-left
-- composition is just regular function composition aka
-- 'Data.Aviary.Birds.bluebird'.
--
(<<<)       :: (b -> c) -> (a -> b) -> (a -> c)
(<<<)       = (Cat.<<<)


-- | For the function instance of Category left-to-right
-- composition is the 'Data.Aviary.Birds.queer' bird.
--
(>>>)       :: (a -> b) -> (b -> c) -> (a -> c)
(>>>)       = (Cat.>>>)

--------------------------------------------------------------------------------
-- Control.Monad
-- The monad here is the environent monad (aka reader)

-- ((->) r) replaces the type variable m

(>>=)       :: (r -> a) -> (a -> r -> b) -> (r -> b)
(>>=)       = (Mon.>>=)

(>>)        :: (r -> a) -> (r -> b) -> (r -> b)
(>>)        = (Mon.>>)


-- | The function instance of Monadic 'return' is equal to the
-- constant function ('const') aka 'Data.Aviary.Birds.kestrel'.
--
return      :: a -> (r -> a)
return      = Mon.return

fail        :: String -> (r -> a)
fail        = fail

-- No function instance of MonadPlus

mapM        :: (a -> r -> b) -> [a] -> r -> [b]
mapM        = Mon.mapM

mapM_       :: (a -> r -> b) -> [a] -> r -> ()
mapM_       = Mon.mapM_

forM        :: [a] -> (a -> r -> b) -> r -> [b]
forM        = Mon.forM

forM_       :: [a] -> (a -> r -> b) -> r -> ()
forM_       = Mon.forM_


sequence    :: [r -> a] -> r -> [a]
sequence    = Mon.sequence

sequence_   :: [r -> a] -> r -> ()
sequence_   = Mon.sequence_


(=<<)       :: (a -> r -> b) -> (r -> a) -> r -> b
(=<<)       = (Mon.=<<)

(>=>)       :: (a -> r -> b) -> (b -> r -> c) -> a -> r -> c
(>=>)       = (Mon.>=>)


(<=<)       :: (b -> r -> c) -> (a -> r -> b) -> a -> r -> c
(<=<)       = (Mon.<=<)


forever     :: (r -> a) -> (r -> b)
forever     = Mon.forever

join        :: (r -> (r -> a)) -> r -> a
join        = Mon.join

filterM     :: (a -> r -> Bool) -> [a] -> r -> [a]
filterM     = Mon.filterM


mapAndUnzipM :: (a -> r -> (b, c)) -> [a] -> r -> ([b], [c])
mapAndUnzipM = Mon.mapAndUnzipM

zipWithM    :: (a -> b -> r -> c) -> [a] -> [b] -> r -> [c]
zipWithM    = Mon.zipWithM

zipWithM_   :: (a -> b -> r -> c) -> [a] -> [b] -> r -> ()
zipWithM_   = Mon.zipWithM_


foldM       :: (a -> b -> r -> a) -> a -> [b] -> r -> a
foldM       = Mon.foldM

foldM_      :: (a -> b -> r -> a) -> a -> [b] -> r -> ()
foldM_      = Mon.foldM_

replicateM  :: Int -> (r -> a) -> r -> [a]
replicateM  = Mon.replicateM

replicateM_ :: Int -> (r -> a) -> r -> ()
replicateM_ = Mon.replicateM_

when        :: Bool -> (r -> ()) -> r -> ()
when        = Mon.when

unless      :: Bool -> (r -> ()) -> r -> ()
unless      = Mon.unless

-- | The Monadic function 'liftM' would ideally be a synonym for
-- 'fmap', so for the function instance of of Monad it corresponds
-- to composition - Haskell\'s ('.') and the
-- 'Data.Aviary.Birds.bluebird' combinator.
--
liftM       :: (a -> b) -> (r -> a) -> r -> b
liftM       = Mon.liftM


-- | 'liftM2' for the function instance of Monad is the
-- 'Data.Aviary.Birds.phoenix' combinator, also called big Phi
-- and /starling-prime/.
--
liftM2      :: (a -> b -> c) -> (r -> a) -> (r -> b) -> r -> c
liftM2      = Mon.liftM2

liftM3      :: (a -> b -> c -> d) -> (r -> a) -> (r -> b) -> (r -> c) -> r -> d
liftM3      = Mon.liftM3

liftM4      :: (a -> b -> c -> d -> e)
            -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) -> r -> e
liftM4      = Mon.liftM4

liftM5      :: (a -> b -> c -> d -> e -> f)
            -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) -> (r -> e) -> r -> f
liftM5      = Mon.liftM5

-- | 'ap' is the Monadic equivalent of the Applicative operator
-- ('<*>'). So for the function instance of Monad it corresponds
-- to the S combinator aka 'Data.Aviary.Birds.starling'.
--
ap          :: (r -> a -> b) -> (r -> a) -> r -> b
ap          = Mon.ap


--------------------------------------------------------------------------------
-- Control.Arrow

-- (->) replaces the type variable a


-- | The Arrow operation 'arr' corresponds to function
-- application for the function instance of Arrow - i.e.
-- Haskell\'s ('$') operator.
--
-- This is the 'Data.Aviary.Birds.applicator' combinator in
-- Data.Aviary.
--
arr         :: (b -> c) -> b -> c
arr         = Arr.arr

first       :: (b -> c) -> (b, d) -> (c, d)
first       = Arr.first

second      :: (b -> c) -> (d, b) -> (d, c)
second      = Arr.second

(***)       :: (b -> c) -> (b' -> c') -> (b, b') -> (c, c')
(***)       = (Arr.***)

(&&&)       :: (b -> c) -> (b -> c') -> b -> (c, c')
(&&&)       = (Arr.&&&)


-- | For the function instance of Arrow, 'returnA' is the
-- identity function aka 'Data.Aviary.Birds.idiot'.
--
returnA     :: (b -> b)
returnA     = Arr.returnA

-- | The Arrow operation /precomposition with a pure function/
-- (left-to-right) is equal to the left-to-right composition
-- operator ('>>>') for function Arrows.
--
-- This corresponds to 'Data.Aviary.Birds.queer'.
--
(^>>)       :: (b -> c) -> (c -> d) -> (b -> d)
(^>>)       = (Arr.^>>)

-- | The Arrow operation /postcomposition with a pure function/
-- (left-to-right) is equal to the left-to-right composition
-- operator ('>>>') for function Arrows.
--
-- This corresponds to 'Data.Aviary.Birds.queer'.
--
(>>^)       :: (b -> c) -> (c -> d) -> (b -> d)
(>>^)       = (Arr.>>^)

-- | The Arrow operation /precomposition with a pure function/
-- (right-to-left) is equal to the right-to-left composition
-- operator ('<<<') for function Arrows, which in turn is equal to
-- regular function composition.
--
-- This corresponds to 'Data.Aviary.Birds.bluebird'.
--
(<<^)       :: (c -> d) -> (b -> c) -> (b -> d)
(<<^)       = (Arr.<<^)

-- | The Arrow operation /postcomposition with a pure function/
-- (right-to-left) is equal to the right-to-left composition
-- operator ('<<<') for function Arrows, which in turn is equal to
-- regular function composition.
--
-- This corresponds to 'Data.Aviary.Birds.bluebird'.
--
(^<<)       :: (c -> d) -> (b -> c) -> (b -> d)
(^<<)       = (Arr.^<<)


-- ArrowChoice

left        :: (b -> c) -> (Either b d) -> (Either c d)
left        = Arr.left

right       :: (b -> c) -> (Either d b) -> (Either d c)
right       = Arr.right

(+++)       :: (b -> c) -> (b' -> c') -> (Either b b') -> (Either c c')
(+++)       = (Arr.+++)

(|||)       :: (b -> d) -> (c -> d) -> (Either b c) -> d
(|||)       = (Arr.|||)


-- ArrowApply

app         :: (b -> c, b) -> c
app         = Arr.app

leftApp     :: (b -> c) -> (Either b d) -> (Either c d)
leftApp     = Arr.leftApp


-- ArrowLoop

loop        :: ((b, d) -> (c, d)) -> b -> c
loop        = Arr.loop

--------------------------------------------------------------------------------
-- Comonad

-- Acknowledgement - the type signatures and definitions are from
-- Category.Extras.


-- The comonad here is the 'anonymous exponent' comonad


-- ((->) m) where m is an instance of Monoid, replaces w

extract         :: Monoid m => (m -> a) -> a
extract w       = w mempty

duplicate       :: Monoid m => (m -> a) -> m -> (m -> a)
duplicate w m   = w . mappend m

extend          :: Monoid m => ((m -> a) -> b) -> (m -> a) -> m -> b
extend wf w m   = wf ((duplicate w) m)

liftW           :: Monoid m => (a -> b) -> (m -> a) -> m -> b
liftW f w m     = f (w m)

(=>>)           :: Monoid m => (m -> a) -> ((m -> a) -> b) -> m -> b
(=>>) w wf m    = wf ((duplicate w) m)

(.>>)           :: Monoid m => (m -> a) -> b -> m -> b
(.>>) w b       = extend (\_ -> b) w

liftCtx         :: Monoid m => (a -> b) -> (m -> a) -> b
liftCtx f w     = extract (fmap f w)

mapW            :: Monoid m => ((m -> a) -> b) -> (m -> [a]) -> [b]
mapW wf w       = step (extract w) where
    step [] = []
    step _  = wf (fmap head w) : mapW wf (fmap tail w)


parallelW       :: Monoid m => (m -> [a]) -> [m -> a]
parallelW ws    = step (extract ws) where
    step [] = []
    step _  = (fmap head ws) : parallelW (fmap tail ws)


unfoldW         :: Monoid m => ((m -> b) -> (a, b)) -> (m -> b) -> [a]
unfoldW wf w    = fst (wf w) : unfoldW wf (extend (snd . wf) w)

sequenceW       :: Monoid m => [(m -> a) -> b] -> (m -> a) -> [b]
sequenceW []     _ = []
sequenceW (f:fs) w = f w : sequenceW fs w
