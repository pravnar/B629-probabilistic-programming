{-# LANGUAGE EmptyDataDecls, GADTs #-}

module Kernels where

import Statistics.Distribution
import Statistics.Distribution.Uniform
import Statistics.Quantile
import Control.Monad.Primitive
import System.Random.MWC
import Data.Number.Erf
import Data.Ratio

class Kernel k where
    sample :: PrimMonad m => k -> Double -> Gen (PrimState m) -> m Double
    run :: PrimMonad m => k -> Double -> Int -> Gen (PrimState m) -> m Double

data MetropolisHastings where
    MH :: (ContDistr t, ContDistr p, ContGen p) => t -> (Double -> p) -> MetropolisHastings

instance Kernel MetropolisHastings where
    sample (MH t c_p) xi g = do
      u <- genContVar (uniformDistr 0 1) g
      xstar <- genContVar (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = (density t xstar) * (density (c_p xstar) xi)
          denom = (density t xi) * (density (c_p xi) xstar)
      if u < accept then return xstar else return xi

    run _ x0 0 _ = return x0
    run mh x0 n g = do
      x <- sample mh x0 g
      run mh x (n-1) g

-- density: 0.3*exp(-0.2*x^2) + 0.7*exp(-0.2(x-10)^2)
data ExampleTargetDistr = ED

-- cumulative: -1.38716 erf(4.47214-0.447214 x)+0.594499 erf(0.447214 x)
instance Distribution ExampleTargetDistr where
    cumulative _ x = -1.38716 * (erf (4.47214 - 0.447214*x))
                     + 0.594499 * (erf 0.447214*x)
                     + 0.594499 + 1.38716

instance ContDistr ExampleTargetDistr where
    density _ x = 0.3 * exp (-0.2*x*x) 
                  + 0.7 * (exp $ -0.2*((x-10)**2))
    
    quantile d p = undefined

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
