{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , Action
               , viz_json
               , print_batch
               , viz_batch
               , MetropolisHastings
               , metropolis_hastings
               , viz_mh
               , SimulatedAnnealing
               , simulated_annealing
               , viz_sa
               , Mixture
               , mixture
               , Temp
               , CoolingSchedule
               , MixRatio
               ) where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

type Rand m = MWC.Gen (PrimState m)
type N = Int
type Action x m a = x -> a -> m a

class Kernel (k :: *) (x :: *) where
    step :: PrimMonad m => k -> x -> Rand m -> m x
    
    walk :: PrimMonad m => k -> x -> N -> Rand m -> a -> Action x m a -> m a
    walk _ _ 0 _ st _ = return st
    walk k x n r st act = do 
      x' <- step k x r
      st' <- act x' st
      walk k x' (n-1) r st' act

-- Actions

id_action :: Monad m => Action x m a
id_action _ = return

skip :: Monad m => Int -> Action x m a -> Action x m a
skip 0 act = act
skip _ _ = id_action

-- Visualization --

viz_json :: [Double] -> Int -> Int -> String
viz_json samplelist current total = "{\"current_sample\": " ++ show current
                                    ++ ", \"total_samples\": " ++ show total
                                    ++ ", \"rvars\": {\"x\": " ++ show samplelist
                                    ++ "}}"

print_batch :: Int -> (x -> Double) -> Action x IO ([x], Int)
print_batch n f x (l, i) 
    | i+1 == n = print (map f $ x:l) >> return ([], 0)
    | otherwise = return (x:l, i+1)

type VizAction x m = Action x m ([x], Int, Int)

viz_batch :: (x -> Double) -> N -> Int -> VizAction x IO
viz_batch f total n x (l, i, current) 
    | i+1 == n = do putStrLn $ viz_json (map f $ x:l) (current*n) total
                    return ([], 0, current+1)
    | otherwise = return (x:l, i+1, current)

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

viz_mh :: N -> Int -> VizAction Double IO
viz_mh = viz_batch id

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

viz_sa :: N -> Int -> VizAction (Double, Temp, CoolingSchedule) IO
viz_sa = viz_batch first

-- Kernel Mixtures --

type MixRatio = Double

data Mixture k l = Mix MixRatio k l

mixture :: (Kernel k x, Kernel l x) => MixRatio -> k -> l -> Mixture k l
mixture = Mix

instance (Kernel k x, Kernel l x) => Kernel (Mixture k l) x where
    step (Mix nu k l) x g = do
      u <- sampleFrom (uniform 0 1) g
      if u < nu then step k x g else step l x g

-- mixture_target :: (Distribution t a) => Mixture k l -> t a
-- mixture_target (Mix _ (MH t _) (MH u _)) = if t == u then t else error ""