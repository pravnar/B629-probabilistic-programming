-- | Definitions from "Stochastic Lambda Calculus and Monads of Probability Distributions"
-- http://www.cs.tufts.edu/~nr/pubs/pmonad.pdf

{-# LANGUAGE RankNTypes #-}
module PM where

import System.Random

type Probability = Double

newtype Support a = Support [a]

newtype Exp a = Exp ((a -> Double) -> Double)

newtype Sample a = Sample (RandomGen g => g -> (a, g))

------------------------- Classes ------------------------------------
class Monad m => ProbabilityMonad m where
  choose :: Probability -> m a -> m a -> m a

class ProbabilityMonad m => SupportMonad m where
  support :: m a -> [a]

class ProbabilityMonad m => ExpMonad m where
  expectation :: (a -> Double) -> m a -> Double

class ProbabilityMonad m => SamplingMonad m where
  sample :: RandomGen g => m a -> g -> (a, g)

------------------------- Instances ------------------------------------
instance Monad Support where
  return x = Support [x]
  (Support l) >>= k = Support (concat [s | x <- l, let Support s = k x])

instance ProbabilityMonad Support where
  choose _ (Support l) (Support l') = Support (l ++ l')

instance SupportMonad Support where
  support (Support l) = l

instance Monad Exp where
  return x = Exp (\h -> h x)
  (Exp d) >>= k =
    Exp (\h -> let apply (Exp f) arg = f arg
                   g x = apply (k x) h
               in d g)

instance ProbabilityMonad Exp where
  choose p (Exp d1) (Exp d2) = Exp (\h -> p * d1 h + (1 - p) * d2 h)

instance ExpMonad Exp where
  expectation h (Exp d) = d h

instance Monad Sample where
  return x = Sample (\g -> (x , g))
  (Sample s) >>= k =
    Sample (\g -> let (a,g') = s g
                      Sample s' = k a
                  in s' g')

instance ProbabilityMonad Sample where
  choose p (Sample s1) (Sample s2) =
    Sample (\g -> let (x, g') = random g
                  in (if x < p then s1 else s2) g')

instance SamplingMonad Sample where
  sample (Sample s) g = s g