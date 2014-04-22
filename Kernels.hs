{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , MetropolisHastings
               , metropolis_hastings
               , Temp
               , CoolingSchedule
               , MixRatio
               , SimulatedAnnealing
               , St_SA
               , simulated_annealing
               , Mixture
               , mixture
               ) where

import Distributions
import Actions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

-- Kernels --

type Rand m = MWC.Gen (PrimState m)
type N = Int

class Kernel (k :: *) (x :: *) where
    step :: PrimMonad m => k -> x -> Rand m -> m x
    
    walk :: PrimMonad m => k -> x -> N -> Rand m -> Action x m a -> m a
    walk _ _ 0 _ (Action _ a) = return a
    walk k x n r action = do 
      x' <- step k x r
      execute action x' >>= walk k x' (n-1) r

-- Metropolis Hastings --

data MetropolisHastings t p a = MH (t a) (a -> p a) 

metropolis_hastings :: t a -> (a -> p a) -> MetropolisHastings t p a
metropolis_hastings = MH

instance (Distribution t a, Sampleable p a) => Kernel (MetropolisHastings t p a) a where
    step (MH t c_p) xi g = do
      u <- sampleFrom (uniform 0 1) g
      xstar <- sampleFrom (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = (density t xstar) * (density (c_p xstar) xi)
          denom = (density t xi) * (density (c_p xi) xstar)
      if u < accept then return xstar else return xi

-- Simulated Annealing --

type Temp = Double
type CoolingSchedule = Temp -> Temp

data SimulatedAnnealing t p a = SA (t a) (a -> p a) 

simulated_annealing :: t a -> (a -> p a) -> SimulatedAnnealing t p a
simulated_annealing = SA

type St_SA a = (a, Temp, CoolingSchedule)

instance (Distribution t a, Sampleable p a) => Kernel (SimulatedAnnealing t p a) (St_SA a) where
    step (SA t c_p) (xi,temp,cool) g = do
      u <- sampleFrom (uniform 0 1) g
      xstar <- sampleFrom (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = (*) (density (c_p xstar) xi) $ (**) (1 / temp) (density t xstar)
          denom = (*) (density (c_p xi) xstar) $ (**) (1 / temp) (density t xi)
          new_temp = cool temp
      if u < accept then return (xstar,new_temp,cool) else return (xi,new_temp,cool)

-- Kernel Mixtures --

type MixRatio = Double

data Mixture k l = Mix MixRatio k l

mixture :: (Kernel k x, Kernel l x) => MixRatio -> k -> l -> Mixture k l
mixture = Mix

instance (Kernel k x, Kernel l x) => Kernel (Mixture k l) x where
    step (Mix nu k l) x g = do
      u <- sampleFrom (uniform 0 1) g
      if u < nu then step k x g else step l x g

