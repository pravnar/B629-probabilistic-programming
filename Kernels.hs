{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Kernels where

data Distribution a

class Kernel k where
    sample :: k a -> a
    -- proposal :: k -> a -> Distribution a
    -- target :: k -> Distribution a

data MetropolisHastings a = MH (Distribution a) (a -> Distribution a)

instance Kernel MetropolisHastings where
    sample _ = undefined

data CoolingSchedule

data SimulatedAnnealing a = SA (Distribution a) (a -> Distribution a) CoolingSchedule

instance Kernel SimulatedAnnealing where
    sample _ = undefined

data Mixture k l a where
    Mixture :: (Kernel k, Kernel l) => k a -> l a -> Mixture k l a

instance Kernel (Mixture k l) where
    sample _ = undefined

-- mixture :: (Kernel k, Kernel l) => k a -> l a -> Mixture k l a
-- mixture = undefined
