{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , Chain
               , St
               , Action
               , print_latest_samples
               , MetropolisHastings
               , metropolis_hastings
               , print_latest_mh_samples
               , SimulatedAnnealing
               , simulated_annealing
               , print_latest_sa_samples
               , Mixture
               , mixture
               , Temp
               , CoolingSchedule
               , MixRatio
               ) where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC
import Control.Monad

type Rand m = MWC.Gen (PrimState m)
type JumpSize = Int
type NumSamples = Int
type Chain = (NumSamples, JumpSize)
type St x = ([x], Either Int Int)
type Action m x = St x -> m (St x)

class Kernel (k :: *) (x :: *) where
    step :: PrimMonad m => k -> x -> Rand m -> m x
            
    nsteps :: PrimMonad m => k -> x -> JumpSize -> Rand m -> m x
    nsteps _ x0 0 _ = return x0
    nsteps k x0 n g = do
      x <- step k x0 g
      nsteps k x (n-1) g
    
    walk :: PrimMonad m => k -> St x -> Chain -> Action m x -> Rand m -> m (St x)
    walk k (xl@(x:_), Left i) (n,m) f rng
        | n == 1 = f (xl, Right i)
        | n > 1 = do xm <- nsteps k x m rng
                     st <- f ((xm:xl), Left (i+1))
                     walk k st ((n-1), m) f rng
        | otherwise = error "Kernel:walk called with #samples < 1"
    walk _ st@(_, Right _) _ _ _ = return st

-- Visualization --

viz_json :: [Double] -> Int -> Int -> String
viz_json samplelist current total = "{\"current_sample\": " ++ show current
                                    ++ ", \"total_samples\": " ++ show total
                                    ++ ", \"rvars\": {\"x\": " ++ show samplelist
                                    ++ "}}"

print_latest_samples :: ([a] -> [Double]) -> Int -> Int -> Action IO a
print_latest_samples f n total st@(xl, Left i) = do
  when (i `mod` n == 0) $ putStrLn (viz_json (take n $ f xl) i total)
  return st
print_latest_samples f n total st@(xl, Right i) = do
  putStrLn (viz_json (take (i `mod` n) $ f xl) total total)
  return st

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

print_latest_mh_samples :: Int -> Int -> Action IO Double
print_latest_mh_samples = print_latest_samples id

-- Simulated Annealing --

type Temp = Double
type CoolingSchedule = Temp -> Temp

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

first :: (a, b, c) -> a
first (a,_,_) = a

print_latest_sa_samples :: Int -> Int -> Action IO (Double, Temp, CoolingSchedule)
print_latest_sa_samples = print_latest_samples (map first)

-- Kernel Mixtures --

type MixRatio = Double

data Mixture k l = Mix MixRatio k l

mixture :: (Kernel k x, Kernel l x) => MixRatio -> k -> l -> Mixture k l
mixture = Mix

instance (Kernel k x, Kernel l x) => Kernel (Mixture k l) x where
    step (Mix nu k l) x g = do
      u <- sampleFrom (uniform 1 0) g
      if u < nu then step k x g else step l x g