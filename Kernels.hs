{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Kernels where

import Data.Random.Distribution as D
import Data.Random.Distribution.Uniform
import Data.Random.RVar

class Kernel k where
    sample :: k a -> RVar a

-- data MetropolisHastings a where
--     MH :: (Distribution p a, Distribution q a) => p a -> (a -> q a) -> MetropolisHastings a

data MetropolisHastings a where
    MH :: (Distribution p a) => p a -> MetropolisHastings a 

-- instance Kernel MetropolisHastings where
--     sample _ = undefined

instance Kernel MetropolisHastings where
    sample (MH t) = do
      u <- stdUniform :: RVar Double
      if u < 0.5 then rvar t else rvar t

data CoolingSchedule

data SimulatedAnnealing a where
    SA :: (Distribution p a, Distribution q a) => p a -> (a -> q a) -> CoolingSchedule -> SimulatedAnnealing a

instance Kernel SimulatedAnnealing where
    sample _ = undefined

data Mixture k l a = Mix (k a) (l a) Double

instance (Kernel k, Kernel l) => Kernel (Mixture k l) where
    sample (Mix ka la nu) = do
      u <- stdUniform
      if u < nu then sample ka else sample la

-- mixture :: (Kernel k, Kernel l) => k a -> l a -> Mixture k l a
-- mixture = undefined
