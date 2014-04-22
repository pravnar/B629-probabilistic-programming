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
               , batch
               , pack
               , batch_print
               , seeMH
               , seeSA
               , print_rem
               ) where

import Distributions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC
import Control.Monad

-- Acts and actions --

type Act x m a = x -> a -> m a

data Action x m a = Action (Act x m a) a

execute :: Monad m => Action x m a -> x -> m (Action x m a)
execute (Action act a) x = act x a >>= return . Action act

skip :: Monad m => Int -> Action x m a -> m (Action x m (a,Int))
skip n (Action act a) = do
  let skip_act x (b,i) = if i == n 
                         then do b' <- act x b
                                 return (b',0)
                         else return (b,i+1)
  return (Action skip_act (a,0))

end_skip :: (a,Int) -> a
end_skip = fst

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

-- Batch actions --  

type Batch x = ([x], Int)
type BatchAct x m = Act x m (Batch x)
type BatchAction x m = Action x m (Batch x)

batch :: Monad m => ([x] -> m s) -> Int -> BatchAct x m
batch f n x (l, i) 
    | i+1 == n = f (x:l) >> return ([], 0)
    | otherwise = return (x:l, i+1)

pack :: BatchAct x m -> BatchAction x m
pack = flip Action ([], 0)

-- Visualization --

type PrintF x = [x] -> [Double]

viz_json :: [Double] -> String
viz_json samplelist = "{\"rvars\": {\"x\": " ++ show samplelist ++ "}}"

visualize :: PrintF x -> [x] -> IO ()
visualize f ls = unless (null ls) $ putStrLn . viz_json $ f ls

batch_print :: PrintF x -> Int -> BatchAction x IO
batch_print f = pack . batch (visualize f)

print_rem :: PrintF x -> Batch x -> IO ()
print_rem f (xs, _) = visualize f xs

-- MH

seeMH :: PrintF Double
seeMH = id

-- SA

first :: (a, b, c) -> a
first (a,_,_) = a

my_filter :: [Double] -> [Double]
my_filter = filter (((>) 40) . abs)

seeSA :: PrintF (St_SA Double)
seeSA = my_filter . map first

