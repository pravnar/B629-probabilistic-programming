{-# LANGUAGE GADTs, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Kernels ( Step
               , Kernel
               , walk
               , metropolisHastings
               , vizMH
               , Temp
               , CoolingSchedule
               , StateSA
               , simulatedAnnealing
               , vizSA
               , mixSteps
               , cycleKernel
               ) where

import Distributions
import Actions

-- Kernels --

type Step x = Rand -> x -> IO x
type Kernel x a = Target a -> (a -> Proposal a) -> Step x

walk :: Step x -> x -> Int -> Rand -> Action x IO a b -> IO b
walk _ _ 0 _ (Action _ f a) = f a
walk step x n r action = do 
  x' <- step r x
  execute action x' >>= walk step x' (n-1) r

-- Metropolis Hastings --

metropolisHastings :: Kernel [a] [a]
metropolisHastings t c_p = 
    let mhStep g xi = do
          u <- sampleFrom (uniform [0] [1]) g
          xstar <- sampleFrom (c_p xi) g
          let accept = min 1 (numer / denom)
              numer = density t xstar * density (c_p xstar) xi
              denom = density t xi * density (c_p xi) xstar
          return $ if head u < accept then xstar else xi
    in mhStep

-- Visualizes only the first dimension
vizMH :: PrintF [Double] Double
vizMH = map head

-- Simulated Annealing --

type Temp = Double
type CoolingSchedule = Temp -> Temp
type StateSA a = (a, Temp, CoolingSchedule)

simulatedAnnealing :: Kernel (StateSA [a]) [a]
simulatedAnnealing t c_p = 
    let saStep g (xi,temp,cool) = do
          u <- sampleFrom (uniform [0] [1]) g
          xstar <- sampleFrom (c_p xi) g
          let accept = min 1 (numer / denom)
              numer = (*) (density (c_p xstar) xi) $ (**) (1 / temp) (density t xstar)
              denom = (*) (density (c_p xi) xstar) $ (**) (1 / temp) (density t xi)
              new_temp = cool temp
          return $ if head u < accept then (xstar,new_temp,cool) else (xi,new_temp,cool)
    in saStep

tripleFirst :: (a, b, c) -> a
tripleFirst (a,_,_) = a

myFilter :: [[Double]] -> [[Double]]
myFilter = filter (\x -> x < (repeat 15) && x > (repeat $ -5))

-- Visualizes only the first dimension
vizSA :: PrintF (StateSA [Double]) Double
vizSA = vizMH . myFilter . map tripleFirst

-- Kernel Mixtures --

mixSteps :: MixRatio -> Step x -> Step x -> Step x
mixSteps nu kstep lstep = 
    let mixStep g x = do
          u <- sampleFrom (uniform [0] [1]) g
          if head u < nu then kstep g x else lstep g x
    in mixStep

-- Kernel Cycles --

cycleKernel :: Kernel x a -> Target a -> [a -> Proposal a] -> Step x
cycleKernel kernel t cps =
  let steps g = [kernel t cp g | cp <- cps]
      combine comb step = (\iox -> iox >>= comb) . step
      cycleStep g = foldl combine return (steps g)
  in cycleStep