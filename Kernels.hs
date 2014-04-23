{-# LANGUAGE GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , MetropolisHastings
               , metropolisHastings
               , vizMH
               , Temp
               , CoolingSchedule
               , MixRatio
               , SimulatedAnnealing
               , StSA
               , simulatedAnnealing
               , vizSA
               , KernelMixture
               , kernelMix
               ) where

import Distributions
import Actions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

-- Kernels --

type Rand m = MWC.Gen (PrimState m)

class Kernel (k :: *) (x :: *) where
    step :: PrimMonad m => k -> x -> Rand m -> m x
    
    walk :: PrimMonad m => k -> x -> Int -> Rand m -> Action x m a b -> m b
    walk _ _ 0 _ (Action _ f a) = f a
    walk k x n r action = do 
      x' <- step k x r
      execute action x' >>= walk k x' (n-1) r

-- Metropolis Hastings --

data MetropolisHastings t p a = MH (t a) (a -> p a) 

metropolisHastings :: t a -> (a -> p a) -> MetropolisHastings t p a
metropolisHastings = MH

instance (Distribution t a, Sampleable p a) => Kernel (MetropolisHastings t p a) a where
    step (MH t c_p) xi g = do
      u <- sampleFrom (uniform 0 1) g
      xstar <- sampleFrom (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = density t xstar * density (c_p xstar) xi
          denom = density t xi * density (c_p xi) xstar
      return $ if u < accept then xstar else xi

vizMH :: PrintF Double
vizMH = id

-- Simulated Annealing --

type Temp = Double
type CoolingSchedule = Temp -> Temp

data SimulatedAnnealing t p a = SA (t a) (a -> p a) 

simulatedAnnealing :: t a -> (a -> p a) -> SimulatedAnnealing t p a
simulatedAnnealing = SA

type StSA a = (a, Temp, CoolingSchedule)

instance (Distribution t a, Sampleable p a) => Kernel (SimulatedAnnealing t p a) (StSA a) where
    step (SA t c_p) (xi,temp,cool) g = do
      u <- sampleFrom (uniform 0 1) g
      xstar <- sampleFrom (c_p xi) g
      let accept = min 1 (numer / denom)
          numer = (*) (density (c_p xstar) xi) $ (**) (1 / temp) (density t xstar)
          denom = (*) (density (c_p xi) xstar) $ (**) (1 / temp) (density t xi)
          new_temp = cool temp
      return $ if u < accept then (xstar,new_temp,cool) else (xi,new_temp,cool)

tripleFirst :: (a, b, c) -> a
tripleFirst (a,_,_) = a

myFilter :: [Double] -> [Double]
myFilter = filter (\x -> x < 15 && x > -5)

vizSA :: PrintF (StSA Double)
vizSA = myFilter . map tripleFirst

-- Kernel Mixtures --

type MixRatio = Double

data KernelMixture k l = KernelMix MixRatio k l

kernelMix :: (Kernel k x, Kernel l x) => MixRatio -> k -> l -> KernelMixture k l
kernelMix = KernelMix

instance (Kernel k x, Kernel l x) => Kernel (KernelMixture k l) x where
    step (KernelMix nu k l) x g = do
      u <- sampleFrom (uniform 0 1) g
      if u < nu then step k x g else step l x g
