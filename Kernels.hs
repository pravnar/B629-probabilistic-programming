{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , Action
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
               , viz_json
               , batch_print
               , mh_print
               , viz_mh
               , sa_print
               , viz_sa
               ) where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC
import Control.Monad

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

-- mixture_target :: (Distribution t a) => Mixture k l -> t a
-- mixture_target (Mix _ (MH t _) (MH u _)) = if t == u then t else error ""

-- Actions --

id_action :: Monad m => Action x m a
id_action _ = return

skip :: Monad m => Int -> Action x m a -> Action x m a
skip 0 act = act
skip _ _ = id_action

-- Visualization --

viz_json :: N -> [Double] -> String
viz_json total samplelist = "{\"total_samples\": " ++ show total
                            ++ ", \"rvars\": {\"x\": " ++ show samplelist
                            ++ "}}"

type PrintAction x = Action x IO ([x], Int)

batch_print :: ([x] -> IO ()) -> Int -> PrintAction x
batch_print printf n x (l, i) 
    | i+1 == n = printf (x:l) >> return ([], 0)
    | otherwise = return (x:l, i+1)

visualize :: (x -> Double) -> N -> [x] -> IO ()
visualize f total = putStrLn . viz_json total . map f

-- MH

mh_print :: N -> [Double] -> IO ()
mh_print total ls = unless (null ls) $ visualize id total ls

viz_mh :: N -> Int -> PrintAction Double
viz_mh = batch_print . mh_print

-- SA

first :: (a, b, c) -> a
first (a,_,_) = a

sa_print :: N -> [(St_SA Double)] -> IO ()
sa_print total ls = unless (null ls) $ visualize first total ls

viz_sa :: N -> Int -> PrintAction (St_SA Double)
viz_sa total = batch_print $ visualize first total