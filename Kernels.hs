{-# LANGUAGE GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , walk
               -- , MetropolisHastings
               , metropolisHastings
               , vizMH
               , Temp
               , CoolingSchedule
               , MixRatio
               -- , SimulatedAnnealing
               , StSA
               , simulatedAnnealing
               , vizSA
               -- , KernelMixture
               , kernelMix
               ) where

import Distributions
import Actions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

-- Kernels --

type Rand m = MWC.Gen (PrimState m)
type Step x m = x -> Rand m -> m x

data Kernel x m where
    K :: Step x m -> Kernel x m

walk :: PrimMonad m => Kernel x m -> x -> Int -> Rand m -> Action x m a b -> m b
walk _ _ 0 _ (Action _ f a) = f a
walk k@(K step) x n r action = do 
  x' <- step x r
  execute action x' >>= walk k x' (n-1) r

-- class Kernel (k :: *) (x :: *) where
--     step :: PrimMonad m => k -> x -> Rand m -> m x
    
--     walk :: PrimMonad m => k -> x -> Int -> Rand m -> Action x m a b -> m b
--     walk _ _ 0 _ (Action _ f a) = f a
--     walk k x n r action = do 
--       x' <- step k x r
--       execute action x' >>= walk k x' (n-1) r

-- Metropolis Hastings --

-- data MetropolisHastings t p a = MH (t a) (a -> p a) 

metropolisHastings :: (Distribution t a, Sampleable p a, PrimMonad m) => 
                      t a -> (a -> p a) -> Kernel a m
metropolisHastings t c_p = 
    let mhStep xi g = do
          u <- sampleFrom (uniform 0 1) g
          xstar <- sampleFrom (c_p xi) g
          let accept = min 1 (numer / denom)
              numer = density t xstar * density (c_p xstar) xi
              denom = density t xi * density (c_p xi) xstar
          return $ if u < accept then xstar else xi
    in K mhStep

-- instance (Distribution t a, Sampleable p a) => Kernel (MetropolisHastings t p a) a where
--     step (MH t c_p) xi g = do
--       u <- sampleFrom (uniform 0 1) g
--       xstar <- sampleFrom (c_p xi) g
--       let accept = min 1 (numer / denom)
--           numer = density t xstar * density (c_p xstar) xi
--           denom = density t xi * density (c_p xi) xstar
--       return $ if u < accept then xstar else xi

vizMH :: PrintF Double Double
vizMH = id

-- Simulated Annealing --

type Temp = Double
type CoolingSchedule = Temp -> Temp
type StSA a = (a, Temp, CoolingSchedule)

-- data SimulatedAnnealing t p a = SA (t a) (a -> p a) 

simulatedAnnealing :: (Distribution t a, Sampleable p a, PrimMonad m) => 
                      t a -> (a -> p a) -> Kernel (StSA a) m
simulatedAnnealing t c_p = 
    let saStep (xi,temp,cool) g = do
          u <- sampleFrom (uniform 0 1) g
          xstar <- sampleFrom (c_p xi) g
          let accept = min 1 (numer / denom)
              numer = (*) (density (c_p xstar) xi) $ (**) (1 / temp) (density t xstar)
              denom = (*) (density (c_p xi) xstar) $ (**) (1 / temp) (density t xi)
              new_temp = cool temp
          return $ if u < accept then (xstar,new_temp,cool) else (xi,new_temp,cool)
    in K saStep

-- instance (Distribution t a, Sampleable p a) => Kernel (SimulatedAnnealing t p a) (StSA a) where
--     step (SA t c_p) (xi,temp,cool) g = do
--       u <- sampleFrom (uniform 0 1) g
--       xstar <- sampleFrom (c_p xi) g
--       let accept = min 1 (numer / denom)
--           numer = (*) (density (c_p xstar) xi) $ (**) (1 / temp) (density t xstar)
--           denom = (*) (density (c_p xi) xstar) $ (**) (1 / temp) (density t xi)
--           new_temp = cool temp
--       return $ if u < accept then (xstar,new_temp,cool) else (xi,new_temp,cool)

tripleFirst :: (a, b, c) -> a
tripleFirst (a,_,_) = a

myFilter :: [Double] -> [Double]
myFilter = filter (\x -> x < 15 && x > -5)

vizSA :: PrintF (StSA Double) Double
vizSA = myFilter . map tripleFirst

-- Kernel Mixtures --

type MixRatio = Double

-- data KernelMixture k l x = KernelMix MixRatio k l

kernelMix :: PrimMonad m => MixRatio -> Kernel x m -> Kernel x m -> Kernel x m
kernelMix nu (K kstep) (K lstep) = 
    let mixStep x g = do
          u <- sampleFrom (uniform 0 1) g
          if u < nu then kstep x g else lstep x g
    in K mixStep

-- instance (Kernel k x, Kernel l x) => Kernel (KernelMixture k l x) x where
--     step (KernelMix nu k l) x g = do
--       u <- sampleFrom (uniform 0 1) g
--       if u < nu then step k x g else step l x g
