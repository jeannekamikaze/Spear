{-# LANGUAGE FlexibleInstances #-}
module Spear.Step
(
    -- * Definitions
    Step
,   Elapsed
,   Dt
    -- * Running
,   runStep
    -- * Constructors
,   step
,   sid
,   spure
,   sfst
,   ssnd
,   sfold
    -- * Combinators
,   (.>)
,   (<.)
,   szip
,   switch
,   multiSwitch
)
where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

type Elapsed = Double
type Dt = Float

-- | A step function.
data Step s e a b =
     Step { runStep :: Elapsed -> Dt -> s -> e -> a -> (b, Step s e a b) }

instance Functor (Step s e a) where
         fmap f (Step s1) = Step $ \elapsed dt g e x ->
              let (a, s') = s1 elapsed dt g e x
              in (f a, fmap f s')

instance Monoid (Step s e a a) where
         mempty = sid

         mappend (Step s1) (Step s2) = Step $ \elapsed dt g e a ->
                 let (b, s1') = s1 elapsed dt g e a
                     (c, s2') = s2 elapsed dt g e b
                 in (c, mappend s1' s2')

-- | Construct a step from a function.
step :: (Elapsed -> Dt -> s -> e -> a -> (b, Step s e a b)) -> Step s e a b
step = Step

-- | Step identity.
sid :: Step s e a a
sid = Step $ \_ _ _ _ a -> (a, sid)

-- | Construct a step from a pure function.
spure :: (a -> b) -> Step s e a b
spure f = Step $ \_ _ _ _ x -> (f x, spure f)

-- | The step that returns the first component in the tuple.
sfst :: Step s e (a,b) a
sfst = spure $ \(a,_) -> a

-- | The step that returns the second component in the tuple.
ssnd :: Step s e (a,b) b
ssnd = spure $ \(_,b) -> b

-- | Construct a step that folds a given list of inputs.
--
-- The step is run N+1 times, where N is the size of the input list.
sfold :: Step s (Maybe e) a a -> Step s [e] a a
sfold s = Step $ \elapsed dt g es a ->
      case es of
           [] ->
              let (b',s') = runStep s elapsed dt g Nothing a
              in (b', sfold s')
           es ->
              let (b',s') = sfold' elapsed dt g s a es
              in (b', sfold s')

sfold' :: Elapsed -> Dt -> s -> Step s (Maybe e) a a -> a -> [e]
       -> (a, Step s (Maybe e) a a)
sfold' elapsed dt g s a es = foldl' f (a',s') es
       where f (a,s) e = runStep s elapsed dt g (Just e) a
             (a',s') = runStep s elapsed dt g Nothing a

-- Combinators

-- | Compose two steps.
(.>) :: Step s e a b -> Step s e b c -> Step s e a c
(Step s1) .> (Step s2) = Step $ \elapsed dt g e a ->
      let (b, s1') = s1 elapsed dt g e a
          (c, s2') = s2 elapsed dt g e b
      in (c, s1' .> s2')

-- | Compose two steps.
(<.) :: Step s e a b -> Step s e c a -> Step s e c b
(<.) = flip (.>)

-- | Evaluate two steps and zip their results.
szip :: (a -> b -> c) -> Step s e d a -> Step s e d b -> Step s e d c
szip f (Step s1) (Step s2) = Step $ \elapsed dt g e d ->
     let (a, s1') = s1 elapsed dt g e d
         (b, s2') = s2 elapsed dt g e d
     in (f a b, szip f s1' s2')

-- | Construct a step that switches between two steps based on input.
--
-- The initial step is the first one.
switch :: Eq e
       => e -> (Step s (Maybe e) a a)
       -> e -> (Step s (Maybe e) a a)
       -> Step s (Maybe e) a a
switch flag1 s1 flag2 s2 = switch' s1 flag1 s1 flag2 s2

switch' :: Eq e
        => (Step s (Maybe e) a a)
        -> e -> (Step s (Maybe e) a a)
        -> e -> (Step s (Maybe e) a a)
        -> Step s (Maybe e) a a
switch' cur flag1 s1 flag2 s2 = Step $ \elapsed dt g e a ->
        case e of
             Nothing ->
                     let (a',s') = runStep cur elapsed dt g Nothing a
                     in (a', switch' s' flag1 s1 flag2 s2)
             Just e' ->
                     let next = if e' == flag1 then s1
                                else if e' == flag2 then s2
                                else cur
                         (a',s') = runStep next elapsed dt g e a
                         in (a', switch' s' flag1 s1 flag2 s2)

-- | Construct a step that switches among multiple steps based on input.
multiSwitch :: (Eq e, Ord e) => [(e, Step s (Maybe e) a a)] -> Step s (Maybe e) a a
multiSwitch xs = multiSwitch' Nothing sid (Map.fromList xs)

multiSwitch' :: (Eq e, Ord e)
             => Maybe e -> Step s (Maybe e) a a -> Map e (Step s (Maybe e) a a)
             -> Step s (Maybe e) a a
multiSwitch' curKey cur m = Step $ \elapsed dt g e a ->
             let singleStep = let (a',s') = runStep cur elapsed dt g e a
                              in (a', multiSwitch' curKey s' m)
             in case e of
                     Nothing -> singleStep
                     Just e' -> case Map.lookup e' m of
                          Nothing -> singleStep
                          Just s ->
                               let (a',s') = runStep s elapsed dt g e a
                                   m' = case curKey of
                                             Nothing  -> m
                                             Just key -> Map.insert key cur m
                               in (a', multiSwitch' e s' m')