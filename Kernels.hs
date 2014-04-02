{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Kernels where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

class Kernel k where
    step :: PrimMonad m => k -> Double -> MWC.Gen (PrimState m) -> m Double
    nsteps :: PrimMonad m => k -> Double -> Int -> MWC.Gen (PrimState m) -> m Double
    walk :: PrimMonad m => k -> [Double] -> Int -> Int -> MWC.Gen (PrimState m) -> m [Double]

data MetropolisHastings where
    MH :: (Distribution t, Sampleable p) => t -> (Double -> p) -> MetropolisHastings

instance Kernel MetropolisHastings where
    step (MH t c_p) xi g = do
      u <- sampleFrom (uniform 0 1) g
      xstar <- sampleFrom (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = (density t xstar) * (density (c_p xstar) xi)
          denom = (density t xi) * (density (c_p xi) xstar)
      if u < accept then return xstar else return xi
    
    nsteps _ x0 0 _ = return x0
    nsteps mh x0 n g = do
      x <- step mh x0 g
      nsteps mh x (n-1) g

    walk _ xl 0 _ _ = return xl
    walk mh (x:xs) n m g = do
      xm <- nsteps mh x m g
      walk mh (xm:x:xs) (n-1) m g

-- cumulative: -1.38716 erf(4.47214-0.447214 x)+0.594499 erf(0.447214 x)
-- instance Distribution ExampleTargetDistr where
--     cumulative _ x = -1.38716 * (erf (4.47214 - 0.447214*x))
--                      + 0.594499 * (erf 0.447214*x)
--                      + 0.594499 + 1.38716

-- data CoolingSchedule

-- data SimulatedAnnealing a where
--     SA :: (Distribution p a, Distribution q a) => p a -> (a -> q a) -> CoolingSchedule -> SimulatedAnnealing a

-- instance Kernel SimulatedAnnealing where
--     sample _ = undefined

-- data Mixture k l a = Mix (k a) (l a) Double

-- instance (Kernel k, Kernel l) => Kernel (Mixture k l) where
--     sample (Mix ka la nu) = do
--       u <- stdUniform
--       if u < nu then sample ka else sample la

-- mixture :: (Kernel k, Kernel l) => k a -> l a -> Mixture k l a
-- mixture = undefined
