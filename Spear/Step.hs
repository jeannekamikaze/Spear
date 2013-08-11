{-# LANGUAGE FlexibleInstances #-}
module Spear.Step
(
    -- * Definitions
    Step(..)
,   Elapsed
,   Dt
    -- * Constructors
,   sid
,   spure
,   sfst
,   ssnd
    -- * Combinators
,   (.>)
,   (<.)
,   szip
)
where

import Data.Monoid

type Elapsed = Double
type Dt = Float

-- | A step function.
data Step a b = Step { step :: Elapsed -> Dt -> a -> (b, Step a b) }

-- | Step identity.
sid :: Step a a
sid = Step $ \_ _ a -> (a, sid)

-- | The step that returns the first component in the tuple.
sfst :: Step (a,b) a
sfst = spure $ \(a,_) -> a

-- | The step that returns the second component in the tuple.
ssnd :: Step (a,b) b
ssnd = spure $ \(_,b) -> b

-- | Construct a step from a pure function.
spure :: (a -> b) -> Step a b
spure f = Step $ \_ _ x -> (f x, spure f)

instance Functor (Step a) where
         fmap f (Step s1) = Step $ \elapsed dt x ->
              let (a, s') = s1 elapsed dt x
              in (f a, fmap f s')

instance Monoid (Step a a) where
         mempty = sid

         mappend (Step s1) (Step s2) = Step $ \elapsed dt a ->
                 let (b, s1') = s1 elapsed dt a
                     (c, s2') = s2 elapsed dt b
                 in (c, mappend s1' s2')

-- Combinators

-- | Chain two steps.
(.>) :: Step a b -> Step b c -> Step a c
(Step s1) .> (Step s2) = Step $ \elapsed dt a ->
      let (b, s1') = s1 elapsed dt a
          (c, s2') = s2 elapsed dt b
      in (c, s1' .> s2')

-- | Chain two steps.
(<.) :: Step a b -> Step c a -> Step c b
(<.) = flip (.>)

-- | Evaluate two steps and zip their results.
szip :: (a -> b -> c) -> Step d a -> Step d b -> Step d c
szip f (Step s1) (Step s2) = Step $ \elapsed dt d ->
     let (a, s1') = s1 elapsed dt d
         (b, s2') = s2 elapsed dt d
     in (f a b, szip f s1' s2')