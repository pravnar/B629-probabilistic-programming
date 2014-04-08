{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , MetropolisHastings
               , metropolis_hastings
               , SimulatedAnnealing
               , simulated_annealing
               , Mixture
               , mixture
               , Temp
               , CoolingSchedule
               , MixRatio
               ) where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

class Kernel (k :: *) (x :: *) where
    step :: PrimMonad m => k -> x -> MWC.Gen (PrimState m) -> m x
    
    nsteps :: PrimMonad m => k -> x -> Int -> MWC.Gen (PrimState m) -> m x
    nsteps _ x0 0 _ = return x0
    nsteps k x0 n g = do
      x <- step k x0 g
      nsteps k x (n-1) g
    
    walk :: PrimMonad m => k -> [x] -> Int -> Int -> MWC.Gen (PrimState m) -> m [x]
    walk _ xl 0 _ _ = return xl
    walk k (x:xs) n m g = do
      xm <- nsteps k x m g
      walk k (xm:x:xs) (n-1) m g

-- data MetropolisHastings a where
--     MH :: (Distribution t a, Sampleable p a) => 
--           t a -> (a -> p a) -> MetropolisHastings a

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

type Temp = Double
type CoolingSchedule = Temp -> Temp

-- data SimulatedAnnealing a where
--     SA :: (Distribution t a, Sampleable p a) => 
--           t a -> (a -> p a) -> SimulatedAnnealing a

data SimulatedAnnealing t p a = SA (t a) (a -> p a) 

simulated_annealing :: t a -> (a -> p a) -> SimulatedAnnealing t p a
simulated_annealing = SA

instance (Distribution t a, Sampleable p a) => Kernel (SimulatedAnnealing t p a) (a, Temp, CoolingSchedule) where
    step (SA t c_p) (xi,temp,cool) g = do
      u <- sampleFrom (uniform 0 1) g
      xstar <- sampleFrom (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = (*) (density (c_p xstar) xi) $ (**) (1 / temp) (density t xstar)
          denom = (*) (density (c_p xi) xstar) $ (**) (1 / temp) (density t xi)
          new_temp = cool temp
      if u < accept then return (xstar,new_temp,cool) else return (xi,new_temp,cool)

type MixRatio = Double

data Mixture k l = Mix MixRatio k l

mixture :: (Kernel k x, Kernel l x) => MixRatio -> k -> l -> Mixture k l
mixture = Mix

instance (Kernel k x, Kernel l x) => Kernel (Mixture k l) x where
    step (Mix nu k l) x g = do
      u <- sampleFrom (uniform 1 0) g
      if u < nu then step k x g else step l x g