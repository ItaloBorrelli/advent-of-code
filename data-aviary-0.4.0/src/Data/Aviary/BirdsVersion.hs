{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.BirdsVersion
-- Copyright   :  (c) Stephen Peter Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Bird monickered combinators - different naming scheme for the 
-- type signatures to Data.Aviary.Birds.
--
-- This module is intended for illustration (the type signatures!) 
-- rather than utility.
--
-- The \'long reach\' Turner set { S, K, I, B, C, S\', B\', C\' }
--
-- The Joy et al. set { S, I, B, C, J(alt), S\', B\', C\', J(alt)\' } 
-- 
-----------------------------------------------------------------------------

module Data.Aviary.BirdsVersion
  ( 
  -- * Data.Function combinators as birds
    idiot
  , kestrel
  , bluebird
  , cardinal
  , applicator
  , psi
  
  -- * Other birds (alphabetical)
  , becard
  , blackbird
  , bluebird'
  , bunting
  , cardinal'
  , cardinalstar
  , cardinalstarstar
  , dove
  , dickcissel
  , dovekie
  , eagle
  , eaglebald
  , finch
  , finchstar
  , finchstarstar
  , goldfinch
  , hummingbird
  , idstar
  , idstarstar
  , jalt
  , jalt'
  , jay
  , kite
  , owl
  , phoenix
  , quacky
  , queer
  , quirky
  , quixotic
  , quizzical
  , robin
  , robinstar
  , robinstarstar
  , starling
  , starling'
  , thrush
  , vireo
  , vireostar
  , vireostarstar
  , warbler
  , warbler1
  , warblerstar
  , warblerstarstar

  ) where

import Data.Function

--------------------------------------------------------------------------------
-- Combinators

-- Bird named versions from Data.Function




-- | I combinator - identity bird / idiot bird - Haskell 'id'.
idiot :: ans -> ans
idiot = id

-- | K combinator - kestrel - Haskell 'const'.
-- Corresponds to the encoding of @true@ in the lambda calculus.
kestrel :: ans -> r1 -> ans
kestrel = const

-- | B combinator - bluebird - Haskell ('.').
bluebird :: (a -> ans) -> (r1 -> a) -> r1 -> ans
bluebird = (.)


-- | C combinator - cardinal - Haskell 'flip'.
cardinal :: (r2 -> r1 -> ans) -> r1 -> r2 -> ans
cardinal = flip

-- | A combinator - apply / applicator - Haskell ('$')
-- 
-- This is also called @i-star@.
applicator :: (r1 -> ans) -> r1 -> ans
applicator = ($)

-- 'fix' - which Y is Haskell\'s fix? (certainly it\'s the least 
-- fixed point)

-- | Psi combinator - psi bird (?) - Haskell 'on'.  
psi :: (a -> a -> ans) -> (r1 -> a) -> r1 -> r1 -> ans
psi = on



--------------------------------------------------------------------------------
-- Other birds



-- | B3 combinator - becard.
becard :: (b -> ans) -> (a -> b) -> (r1 -> a) -> r1 -> ans
becard f g h x = f (g (h x))

-- | B1 combinator - blackbird - specs 'oo'.
blackbird :: (a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
blackbird f g x y = f (g x y)

-- | B' combinator - bluebird prime.
bluebird' :: (r1 -> a -> ans) -> r1 -> (r2 -> a) -> r2 -> ans
bluebird' f x g y = f x (g y)

-- | B2 combinator - bunting - specs 'ooo'.
bunting :: (a -> ans) -> (r1 -> r2 -> r3 -> a) -> r1 -> r2 -> r3 -> ans
bunting f g x y z = f (g x y z)



-- | C' combinator - no name.
cardinal' :: (a -> r1 -> ans) -> (r2 -> a) -> r1 -> r2 -> ans
cardinal' f g x y = f (g y) x

-- | C* combinator - cardinal once removed.
cardinalstar :: (r1 -> r3 -> r2 -> ans) -> r1 -> r2 -> r3 -> ans
cardinalstar f x y z = f x z y

-- | C** combinator - cardinal twice removed.
cardinalstarstar :: (r1 -> r2 -> r4 -> r3 -> ans) -> r1 -> r2 -> r3 -> r4 -> ans
cardinalstarstar f x y z1 z2 = f x y z2 z1


-- | D1 combinator - dickcissel.
dickcissel :: (r1 -> r2 -> a -> ans) -> r1 -> r2 -> (r3 -> a) -> r3 -> ans
dickcissel f x y g z = f x y (g z)


-- | D combinator - dove.
dove :: (r1 -> a -> ans) -> r1 -> (r2 -> a) -> r2 -> ans
dove f x g y = f x (g y)


-- | D2 combinator - dovekie.
dovekie :: (a -> b -> ans) -> (r1 -> a) -> r1 -> (r2 -> b) -> r2 -> ans
dovekie f g x h z = f (g x) (h z)

-- | E combinator - eagle.
eagle :: (a -> d -> ans) -> a -> (b -> c -> d) -> b -> c -> ans
eagle f x g y z = f x (g y z) 

-- | E \^ - bald eagle.
-- For alphabetical regularity it is somewhat misnamed here as 
-- eaglebald.
eaglebald :: (a -> b -> ans) 
          -> (r1 -> r2 -> a) 
          -> r1 -> r2 
          -> (r3 -> r4 -> b) 
          -> r3 -> r4 -> ans  
eaglebald f g x y h z1 z2 = f (g x y) (h z1 z2)



-- | F combinator - finch.
finch :: r1 -> r2 -> (r2 -> r1 -> ans) -> ans
finch x y f = f y x

-- | F* combinator - finch once removed.
finchstar :: (r3 -> r2 -> r1 -> ans) -> r1 -> r2 -> r3 -> ans
finchstar f x y z = f z y x

-- | F** combinator - finch twice removed.
finchstarstar :: (r1 -> r4 -> r3 -> r2 -> ans) -> r1 -> r2 -> r3 -> r4 -> ans
finchstarstar f x y z1 z2 = f x z2 z1 y


-- | G combinator - goldfinch.
goldfinch :: (r2 -> a -> ans) -> (r1 -> a) -> r1 -> r2 -> ans
goldfinch f g x y = f y (g x)

-- | H combinator - hummingbird.
hummingbird :: (r1 -> r2 -> r1 -> ans) -> r1 -> r2 -> ans
hummingbird f x y = f x y x


-- | I* combinator - identity bird once removed
-- Alias of 'applicator', Haskell\'s ('$').
idstar :: (r1 -> ans) -> r1 -> ans
idstar f x = f x

-- | I** combinator - identity bird twice removed
idstarstar :: (r1 -> r2 -> ans) -> r1 -> r2 -> ans
idstarstar f x y = f x y


-- | Alternative J combinator - this is the J combintor of Joy,
-- Rayward-Smith and Burton (see. Antoni Diller \'Compiling 
-- Functional Languages\' page 104). It is not the J - jay 
-- combinator of the literature.   
jalt :: (r1 -> ans) -> r1 -> r2 -> ans
jalt f x _y = f x


-- | J' combinator - from Joy, Rayward-Smith and Burton.
-- See the comment to 'jalt'.
jalt' :: (r1 -> r2 -> ans) -> r1 -> r2 -> r3 -> ans
jalt' f x y _z = f x y

-- | J combinator - jay.
--
-- This is the usual J combinator.
jay :: (r1 -> ans -> ans) -> r1 -> ans -> r1 -> ans
jay f x y z = f x (f z y)


-- | Ki - kite.
-- Corresponds to the encoding of @false@ in the lambda calculus.
kite :: r1 -> ans -> ans
kite _x y = y

-- | O combinator - owl.
owl :: ((a -> ans) -> a) -> (a -> ans) -> ans
owl x y = y (x y)


-- | (Big) Phi combinator - phoenix - Haskell 'liftM2'.
phoenix :: (a -> b -> ans) -> (r1 -> a) -> (r1 -> b) -> r1 -> ans
phoenix f g h x = f (g x) (h x)


-- | Q4 combinator - quacky bird.
quacky :: r1 -> (r1 -> a) -> (a -> ans) -> ans
quacky x f g = g (f x)

-- | Q combinator - queer bird.
--
-- Haskell @(\#\#)@ in Peter Thiemann\'s Wash, reverse composition.
queer :: (r1 -> a) -> (a -> ans) -> r1 -> ans
queer f g x = g (f x)

-- | Q3 combinator - quirky bird.
quirky :: (r1 -> a) -> r1 -> (a -> ans) -> ans
quirky f x g = g (f x)


-- | Q1 combinator - quixotic bird.
quixotic :: (a -> ans) -> r1 -> (r1 -> a) -> ans
quixotic f x g = f (g x)

-- | Q2 combinator - quizzical bird.
quizzical :: r1 -> (a -> ans) -> (r1 -> a) -> ans
quizzical x f g = f (g x)


-- | R combinator - robin.
robin :: r1 -> (r2 -> r1 -> ans) -> r2 -> ans
robin x f y = f y x 


-- | R* combinator - robin once removed.
robinstar :: (r2 -> r3 -> r1 -> ans) -> r1 -> r2 -> r3 -> ans
robinstar f x y z = f y z x

-- | R** combinator - robin twice removed.
robinstarstar :: (r1 -> r3 -> r4 -> r2 -> ans) -> r1 -> r2 -> r3 -> r4 -> ans
robinstarstar f x y z1 z2 = f x z1 z2 y 

-- | S combinator - starling. 
-- 
-- Haskell: Applicative\'s @(\<*\>)@ on functions.
--
-- (Substitution / composition).
starling :: (r1 -> a -> ans) -> (r1 -> a) -> r1 -> ans
starling f g x = f x (g x)


-- | S' combinator - starling prime - Turner\'s big phi. 
-- Haskell: Applicative\'s liftA2 on functions.
starling' :: (a -> b -> ans) -> (r1 -> a) -> (r1 -> b) -> r1 -> ans
starling' f g h x = f (g x) (h x)


-- | T combinator - thrush.
-- Haskell @(\#)@ in Peter Thiemann\'s Wash, reverse application.
thrush :: r1 -> (r1 -> ans) -> ans
thrush x f = f x

-- | V combinator - vireo.
vireo :: r1 -> r2 -> (r1 -> r2 -> ans) -> ans
vireo x y f = f x y

-- | V* combinator - vireo once removed.
vireostar :: (r2 -> r1 -> r2 -> ans) -> r1 -> r2 -> r2 -> ans
vireostar f x y z = f y x z

-- | V** combinator - vireo twice removed.
vireostarstar :: (r1 -> r3 -> r2 -> r3 -> ans) -> r1 -> r2 -> r3 -> r3 -> ans
vireostarstar f x y z1 z2 = f x z2 y z1


-- | W combinator - warbler - elementary duplicator.
warbler :: (r1 -> r1 -> ans) -> r1 -> ans
warbler f x = f x x

-- | W1 combinator - converse warbler.
-- 'warbler' with the arguments reversed.
warbler1 :: r1 -> (r1 -> r1 -> ans) -> ans
warbler1 x f = f x x

-- | W* combinator - warbler once removed.
warblerstar :: (r1 -> r2 -> r2 -> ans) -> r1 -> r2 -> ans
warblerstar f x y = f x y y

-- | W** combinator - warbler twice removed.
warblerstarstar :: (r1 -> r2 -> r3 -> r3 -> ans) -> r1 -> r2 -> r3 -> ans
warblerstarstar f x y z = f x y z z
