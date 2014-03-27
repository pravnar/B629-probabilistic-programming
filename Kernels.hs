{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Kernels where

import Data.Random.Distribution as D

class Kernel k where
    sample :: k a -> a

data MetropolisHastings a where
    MH :: (Distribution p a, Distribution q a) => p a -> (a -> q a) -> MetropolisHastings a

instance Kernel MetropolisHastings where
    sample _ = undefined

data CoolingSchedule

data SimulatedAnnealing a where
    SA :: (Distribution p a, Distribution q a) => p a -> (a -> q a) -> CoolingSchedule -> SimulatedAnnealing a

instance Kernel SimulatedAnnealing where
    sample _ = undefined

data Mixture k l a where
    Mixture :: (Kernel k, Kernel l) => k a -> l a -> Mixture k l a

instance Kernel (Mixture k l) where
    sample _ = undefined

-- mixture :: (Kernel k, Kernel l) => k a -> l a -> Mixture k l a
-- mixture = undefined
