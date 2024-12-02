{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aviary.BirdsInter
-- Copyright   :  (c) Stephen Peter Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Bird monickered combinators interdefined.
-- 
-- This module is intended for illustration rather than utility.
-- 
-- The \'long reach\' Turner set { S, K, I, B, C, S\', B\', C\' }
--
-- The Joy et al. set { S, I, B, C, J(alt), S\', B\', C\', J(alt)\' } 
--
-----------------------------------------------------------------------------

module Data.Aviary.BirdsInter
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



--------------------------------------------------------------------------------
-- Combinators

-- Bird named versions from Data.Function




-- | I combinator - identity bird / idiot bird - Haskell 'id'.
idiot :: a -> a 
idiot = starling kestrel kestrel 

-- | K combinator - kestrel - Haskell 'const'.
-- Corresponds to the encoding of @true@ in the lambda calculus.
--
-- Not interdefined.
kestrel :: a -> b -> a
kestrel a _b = a 

-- | B combinator - bluebird - Haskell ('.').
bluebird :: (b -> c) -> (a -> b) -> a -> c
bluebird = starling (kestrel starling) kestrel


-- | C combinator - cardinal - Haskell 'flip'.
cardinal :: (a -> b -> c) -> b -> a -> c
cardinal = starling (bluebird bluebird starling) (kestrel kestrel)

-- | A combinator - apply / applicator - Haskell ('$').
--
-- This is also called @i-star@.
--
-- Note: the (inter-) definition here is @- C (B B I) I -@ and 
-- not the familiar @- S (S K) -@ which as far as Haskell is 
-- concerned has a different type:
-- 
-- > (S(SK)) :: ((a -> b) -> a) -> (a -> b) -> a 
--
applicator :: (a -> b) -> a -> b
applicator = cardinal (bluebird bluebird idiot) idiot



-- 'fix' - which Y is Haskell\'s fix? (certainly it\'s the least 
-- fixed point)

-- | Psi combinator - psi bird (?) - Haskell 'on'.  
psi :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi = cardinal (bluebird starling (bluebird cardinalstar dovekie)) applicator



--------------------------------------------------------------------------------
-- Other birds



-- | B3 combinator - becard.
becard :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
becard = bluebird (bluebird bluebird) bluebird

-- | B1 combinator - blackbird - specs 'oo'.
blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
blackbird = bluebird bluebird bluebird


-- | B' combinator - bluebird prime.
bluebird' :: (a -> c -> d) -> a -> (b -> c) -> b -> d
bluebird' = bluebird bluebird


-- | B2 combinator - bunting - specs 'ooo'.
bunting :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
bunting = bluebird (bluebird bluebird bluebird) bluebird



-- | C' combinator - no name.
cardinal' :: (c -> a -> d) -> (b -> c) -> a -> b -> d
cardinal' = bluebird (bluebird cardinal) bluebird

-- | C* combinator - cardinal once removed.
cardinalstar :: (a -> c -> b -> d) -> a -> b -> c -> d
cardinalstar = bluebird cardinal

-- | C** combinator - cardinal twice removed.
cardinalstarstar :: (a -> b -> d -> c -> e) -> a -> b -> c -> d -> e
cardinalstarstar = bluebird cardinalstar


-- | D1 combinator - dickcissel.
dickcissel :: (a -> b -> d -> e) -> a -> b -> (c -> d) -> c -> e
dickcissel = bluebird (bluebird bluebird)


-- | D combinator - dove.
dove :: (a -> c -> d) -> a -> (b -> c) -> b -> d
dove = bluebird bluebird


-- | D2 combinator - dovekie.
dovekie :: (c -> d -> e) -> (a -> c) -> a -> (b -> d) -> b -> e
dovekie = bluebird bluebird (bluebird bluebird)

-- | E combinator - eagle.
eagle :: (a -> d -> e) -> a -> (b -> c -> d) -> b -> c -> e
eagle = bluebird (bluebird bluebird bluebird) 


-- | E \^ - bald eagle.
-- For alphabetical regularity it is somewhat misnamed here as 
-- eaglebald.
eaglebald :: (e -> f -> g) 
          -> (a -> b -> e) 
          -> a -> b 
          -> (c -> d -> f) 
          -> c -> d -> g  
eaglebald = 
    bluebird (bluebird bluebird bluebird) (bluebird (bluebird bluebird bluebird))



-- | F combinator - finch.
finch :: a -> b -> (b -> a -> c) -> c
finch = eagle thrush thrush eagle thrush

-- | F* combinator - finch once removed.
finchstar :: (c -> b -> a -> d) -> a -> b -> c -> d
finchstar = bluebird cardinalstar robinstar 

-- | F** combinator - finch twice removed.
finchstarstar :: (a -> d -> c -> b -> e) -> a -> b -> c -> d -> e
finchstarstar = bluebird finchstar


-- | G combinator - goldfinch.
goldfinch :: (b -> c -> d) -> (a -> c) -> a -> b -> d
goldfinch = bluebird bluebird cardinal

-- | H combinator - hummingbird.
hummingbird :: (a -> b -> a -> c) -> a -> b -> c 
hummingbird = bluebird warbler (bluebird cardinal)

-- | I* combinator - identity bird once removed.
-- Alias of 'applicator', Haskell\'s ('$').
-- Type signature 
idstar :: (a -> b) -> a -> b
idstar = cardinal (bluebird bluebird idiot) idiot


-- | I** combinator - identity bird twice removed.
idstarstar :: (a -> b -> c) -> a -> b -> c
idstarstar = bluebird idstar

-- | Alternative J combinator - this is the J combintor of Joy,
-- Rayward-Smith and Burton (see. Antoni Diller \'Compiling 
-- Functional Languages\' page 104). It is not the J - jay 
-- combinator of the literature.   
jalt :: (a -> c) -> a -> b -> c
jalt = bluebird kestrel


-- | J' combinator - from Joy, Rayward-Smith and Burton.
-- See the comment to 'jalt'.
jalt' :: (a -> b -> d) -> a -> b -> c -> d
jalt' = bluebird (bluebird kestrel)


-- | J combinator - jay.
--
-- This is the usual J combinator.
jay :: (a -> b -> b) -> a -> b -> a -> b
jay = bluebird (bluebird cardinal) 
               (warbler (bluebird cardinal 
                                  (bluebird (bluebird bluebird bluebird))))


-- | Ki - kite.
-- Corresponds to the encoding of @false@ in the lambda calculus.
kite :: a -> b -> b
kite = kestrel idiot

-- | O combinator - owl.
owl :: ((a -> b) -> a) -> (a -> b) -> b
owl = starling idiot


-- | (Big) Phi combinator - phoenix - Haskell 'liftM2'.
phoenix :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
phoenix = bluebird (bluebird starling) bluebird


-- | Q4 combinator - quacky bird.
quacky :: a -> (a -> b) -> (b -> c) -> c 
quacky = finchstar bluebird

-- | Q combinator - queer bird.
--
-- Haskell @(\#\#)@ in Peter Thiemann\'s Wash, reverse composition.
queer :: (a -> b) -> (b -> c) -> a -> c
queer = cardinal bluebird

-- | Q3 combinator - quirky bird.
quirky :: (a -> b) -> a -> (b -> c) -> c
quirky = bluebird thrush

-- | Q1 combinator - quixotic bird.
quixotic :: (b -> c) -> a -> (a -> b) -> c
quixotic = bluebird cardinal bluebird

-- | Q2 combinator - quizzical bird.
quizzical :: a -> (b -> c) -> (a -> b) -> c
quizzical = cardinal (bluebird cardinal bluebird)


-- | R combinator - robin.
robin :: a -> (b -> a -> c) -> b -> c
robin = bluebird bluebird thrush


-- | R* combinator - robin once removed.
robinstar :: (b -> c -> a -> d) -> a -> b -> c -> d
robinstar = cardinalstar cardinalstar

-- | R** combinator - robin twice removed.
robinstarstar :: (a -> c -> d -> b -> e) -> a -> b -> c -> d -> e
robinstarstar = bluebird robinstar


-- | S combinator - starling. 
-- 
-- Haskell: Applicative\'s @(\<*\>)@ on functions.
--
-- Not interdefined.
starling :: (a -> b -> c) -> (a -> b) -> a -> c
starling f g x = f x (g x)


-- | S' combinator - starling prime - Turner\'s big phi. 
-- Haskell: Applicative\'s liftA2 on functions.
starling' :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
starling' = bluebird (bluebird starling) bluebird


-- | T combinator - thrush.
-- Haskell @(\#)@ in Peter Thiemann\'s Wash, reverse application.
thrush :: a -> (a -> b) -> b
thrush = cardinal idiot

-- | V combinator - vireo.
vireo :: a -> b -> (a -> b -> b) -> b
vireo = bluebird cardinal thrush

-- | V* combinator - vireo once removed.
vireostar :: (b -> a -> b -> d) -> a -> b -> b -> d
vireostar = cardinalstar finchstar

-- | V** combinator - vireo twice removed.
vireostarstar :: (a -> c -> b -> c -> e) -> a -> b -> c -> c -> e
vireostarstar = bluebird vireostar


-- | W combinator - warbler - elementary duplicator.
warbler :: (a -> a -> b) -> a -> b
warbler = cardinal starling idiot

-- | W1 combinator - converse warbler.
-- 'warbler' with the arguments reversed.
warbler1 :: a -> (a -> a -> b) -> b
warbler1 = cardinal warbler

-- | W* combinator - warbler once removed.
warblerstar :: (a -> b -> b -> c) -> a -> b -> c
warblerstar = bluebird warbler

-- | W** combinator - warbler twice removed.
warblerstarstar :: (a -> b -> c -> c -> d) -> a -> b -> c -> d
warblerstarstar = bluebird warblerstar
