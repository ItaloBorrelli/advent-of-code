{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.Birds
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Bird monickered combinators
--
-- This module is intended for illustration (the type signatures!) 
-- rather than utility.
--
-- The \'long reach\' Turner set { S, K, I, B, C, S\', B\', C\' }
--
-- The Joy et al. set { S, I, B, C, J(alt), S\', B\', C\', J(alt)\' } 
-- 
-----------------------------------------------------------------------------

module Data.Aviary.Birds
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
idiot :: a -> a 
idiot = id

-- | K combinator - kestrel - Haskell 'const'.
-- Corresponds to the encoding of @true@ in the lambda calculus.
kestrel :: a -> b -> a
kestrel = const

-- | B combinator - bluebird - Haskell ('.').
bluebird :: (b -> c) -> (a -> b) -> a -> c
bluebird = (.)


-- | C combinator - cardinal - Haskell 'flip'.
cardinal :: (a -> b -> c) -> b -> a -> c
cardinal = flip

-- | A combinator - apply / applicator - Haskell ('$').
--
-- This is also called @i-star@.
applicator :: (a -> b) -> a -> b
applicator = ($)

-- 'fix' - which Y is Haskell\'s fix? (certainly it\'s the least 
-- fixed point)

-- | Psi combinator - psi bird (?) - Haskell 'on'.  
psi :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi = on



--------------------------------------------------------------------------------
-- Other birds



-- | B3 combinator - becard.
becard :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
becard f g h x = f (g (h x))

-- | B1 combinator - blackbird - specs 'oo'.
blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
blackbird f g x y = f (g x y)

-- | B' combinator - bluebird prime.
bluebird' :: (a -> c -> d) -> a -> (b -> c) -> b -> d
bluebird' f x g y = f x (g y)

-- | B2 combinator - bunting - specs 'ooo'.
bunting :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
bunting f g x y z = f (g x y z)



-- | C' combinator - no name.
cardinal' :: (c -> a -> d) -> (b -> c) -> a -> b -> d
cardinal' f g x y = f (g y) x

-- | C* combinator - cardinal once removed.
cardinalstar :: (a -> c -> b -> d) -> a -> b -> c -> d
cardinalstar f x y z = f x z y

-- | C** combinator - cardinal twice removed.
cardinalstarstar :: (a -> b -> d -> c -> e) -> a -> b -> c -> d -> e
cardinalstarstar f s t u v = f s t v u


-- | D1 combinator - dickcissel.
dickcissel :: (a -> b -> d -> e) -> a -> b -> (c -> d) -> c -> e
dickcissel f x y g z = f x y (g z)


-- | D combinator - dove.
dove :: (a -> c -> d) -> a -> (b -> c) -> b -> d
dove f x g y = f x (g y)


-- | D2 combinator - dovekie.
dovekie :: (c -> d -> e) -> (a -> c) -> a -> (b -> d) -> b -> e
dovekie f g x h z = f (g x) (h z)

-- | E combinator - eagle.
eagle :: (a -> d -> e) -> a -> (b -> c -> d) -> b -> c -> e
eagle f x g y z = f x (g y z) 

-- | E \^ - bald eagle.
-- For alphabetical regularity it is somewhat misnamed here as 
-- eaglebald.
eaglebald :: (e -> f -> g) 
          -> (a -> b -> e) 
          -> a -> b 
          -> (c -> d -> f) 
          -> c -> d -> g  
eaglebald f g s t h u v = f (g s t) (h u v)



-- | F combinator - finch.
finch :: a -> b -> (b -> a -> c) -> c
finch x y f = f y x

-- | F* combinator - finch once removed.
finchstar :: (c -> b -> a -> d) -> a -> b -> c -> d
finchstar f x y z = f z y x

-- | F** combinator - finch twice removed.
finchstarstar :: (a -> d -> c -> b -> e) -> a -> b -> c -> d -> e
finchstarstar f s t u v = f s v u t


-- | G combinator - goldfinch.
goldfinch :: (b -> c -> d) -> (a -> c) -> a -> b -> d
goldfinch f g x y = f y (g x)

-- | H combinator - hummingbird.
hummingbird :: (a -> b -> a -> c) -> a -> b -> c 
hummingbird f x y = f x y x


-- | I* combinator - identity bird once removed
-- Alias of 'applicator', Haskell\'s ('$').
idstar :: (a -> b) -> a -> b
idstar f x = f x

-- | I** combinator - identity bird twice removed
idstarstar :: (a -> b -> c) -> a -> b -> c
idstarstar f x y = f x y


-- | Alternative J combinator - this is the J combintor of Joy,
-- Rayward-Smith and Burton (see. Antoni Diller \'Compiling 
-- Functional Languages\' page 104). It is not the J - jay 
-- combinator of the literature.   
jalt :: (a -> c) -> a -> b -> c
jalt f x _y = f x


-- | J' combinator - from Joy, Rayward-Smith and Burton.
-- See the comment to 'jalt'.
jalt' :: (a -> b -> d) -> a -> b -> c -> d
jalt' f x y _z = f x y

-- | J combinator - jay.
--
-- This is the usual J combinator.
jay :: (a -> b -> b) -> a -> b -> a -> b
jay f x y z = f x (f z y)


-- | Ki - kite.
-- Corresponds to the encoding of @false@ in the lambda calculus.
kite :: a -> b -> b
kite _x y = y

-- | O combinator - owl.
owl :: ((a -> b) -> a) -> (a -> b) -> b
owl x y = y (x y)


-- | (Big) Phi combinator - phoenix - Haskell 'liftM2'.
--
-- This is the same function as 'Data.Aviary.Birds.starling''. 
-- 
phoenix :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
phoenix f g h x = f (g x) (h x)


-- | Q4 combinator - quacky bird.
quacky :: a -> (a -> b) -> (b -> c) -> c 
quacky x f g = g (f x)

-- | Q combinator - queer bird.
--
-- Haskell @(\#\#)@ in Peter Thiemann\'s Wash, reverse composition.
queer :: (a -> b) -> (b -> c) -> a -> c
queer f g x = g (f x)

-- | Q3 combinator - quirky bird.
quirky :: (a -> b) -> a -> (b -> c) -> c
quirky f x g = g (f x)


-- | Q1 combinator - quixotic bird.
quixotic :: (b -> c) -> a -> (a -> b) -> c
quixotic f x g = f (g x)

-- | Q2 combinator - quizzical bird.
quizzical :: a -> (b -> c) -> (a -> b) -> c
quizzical x f g = f (g x)


-- | R combinator - robin.
robin :: a -> (b -> a -> c) -> b -> c
robin x f y = f y x 


-- | R* combinator - robin once removed.
robinstar :: (b -> c -> a -> d) -> a -> b -> c -> d
robinstar f x y z = f y z x

-- | R** combinator - robin twice removed.
robinstarstar :: (a -> c -> d -> b -> e) -> a -> b -> c -> d -> e
robinstarstar f s t u v = f s u v t

-- | S combinator - starling. 
-- 
-- Haskell: Applicative\'s @(\<*\>)@ on functions.
--
-- Substitution.
starling :: (a -> b -> c) -> (a -> b) -> a -> c
starling f g x = f x (g x)


-- | S' combinator - starling prime - Turner\'s big phi. 
-- Haskell: Applicative\'s 'liftA2' on functions (and similarly 
-- Monad\'s 'liftM2').
--
-- This is the same function as 'Data.Aviary.Birds.phoenix'. 
-- 
starling' :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
starling' f g h x = f (g x) (h x)


-- | T combinator - thrush.
-- Haskell @(\#)@ in Peter Thiemann\'s Wash, reverse application.
thrush :: a -> (a -> b) -> b
thrush x f = f x

-- | V combinator - vireo (pairing).
vireo :: a -> b -> (a -> b -> c) -> c
vireo x y f = f x y

-- | V* combinator - vireo once removed.
vireostar :: (b -> a -> b -> d) -> a -> b -> b -> d
vireostar f x y z = f y x z

-- | V** combinator - vireo twice removed.
vireostarstar :: (a -> c -> b -> c -> e) -> a -> b -> c -> c -> e
vireostarstar f s t u v = f s v t u


-- | W combinator - warbler - elementary duplicator.
warbler :: (a -> a -> b) -> a -> b
warbler f x = f x x

-- | W1 combinator - converse warbler.
-- 'warbler' with the arguments reversed.
warbler1 :: a -> (a -> a -> b) -> b
warbler1 x f = f x x

-- | W* combinator - warbler once removed.
warblerstar :: (a -> b -> b -> c) -> a -> b -> c
warblerstar f x y = f x y y

-- | W** combinator - warbler twice removed.
warblerstarstar :: (a -> b -> c -> c -> d) -> a -> b -> c -> d
warblerstarstar f x y z = f x y z z
