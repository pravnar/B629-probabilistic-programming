{-# LANGUAGE EmptyDataDecls, GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Kernel (..)
               , Action
               , skip
               , end_skip
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
               , mh_batch_viz
               , sa_print
               , sa_batch_viz
               ) where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC
import Control.Monad

-- Actions --

type Action x m a = x -> a -> m a

data Act x m a = Act (Action x m a) a

execute :: Monad m => Act x m a -> x -> m (Act x m a)
execute (Act action a) x = action x a >>= return . Act action

skip :: Monad m => Int -> Act x m a -> m (Act x m (a,Int))
skip n (Act action a) = do
  let skip_action x (b,i) = if i == n 
                            then do b' <- action x b
                                    return (b',0)
                            else return (b,i+1)
  return (Act skip_action (a,0))

end_skip :: (a,Int) -> a
end_skip = fst

-- Kernels --

type Rand m = MWC.Gen (PrimState m)
type N = Int

class Kernel (k :: *) (x :: *) where
    step :: PrimMonad m => k -> x -> Rand m -> m x
    
    walk :: PrimMonad m => k -> x -> N -> Rand m -> Act x m a -> m a
    walk _ _ 0 _ (Act _ a) = return a
    walk k x n r act = do 
      x' <- step k x r
      execute act x' >>= walk k x' (n-1) r

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

-- Visualization --

viz_json :: N -> [Double] -> String
viz_json total samplelist = "{\"total_samples\": " ++ show total
                            ++ ", \"rvars\": {\"x\": " ++ show samplelist
                            ++ "}}"

type Print x = ([x], Int)
type PrintAction x = Action x IO (Print x)

batch_print :: ([x] -> IO ()) -> Int -> PrintAction x
batch_print printf n x (l, i) 
    | i+1 == n = printf (x:l) >> return ([], 0)
    | otherwise = return (x:l, i+1)

visualize :: ([x] -> [Double]) -> N -> [x] -> IO ()
visualize f total = putStrLn . viz_json total . f

pack :: PrintAction x -> Act x IO (Print x)
pack = flip Act ([], 0)

-- MH

mh_print :: N -> [Double] -> IO ()
mh_print total ls = unless (null ls) $ visualize id total ls

mh_batch_viz :: N -> Int -> Act Double IO (Print Double)
mh_batch_viz total = pack . batch_print (mh_print total)

-- SA

first :: (a, b, c) -> a
first (a,_,_) = a

my_filter :: [Double] -> [Double]
my_filter = filter (((>) 40) . abs)

sa_print :: N -> [(St_SA Double)] -> IO ()
sa_print total ls = unless (null ls) $ visualize (my_filter . map first) total ls

sa_batch_viz :: N -> Int -> Act (St_SA Double) IO (Print (St_SA Double))
sa_batch_viz total = pack . batch_print (sa_print total)
