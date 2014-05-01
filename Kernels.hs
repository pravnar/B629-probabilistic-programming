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
               ) where

import Distributions
import Actions
import Control.Monad.Primitive
import qualified System.Random.MWC as MWC

-- Kernels --

type Step x = x -> Rand -> IO x
type Kernel x a = Target a -> (Sample a -> Proposal a) -> Step x

-- data Kernel x where
--     K :: (AbsCont t a, Sampleable p a) => Step x t p a -> Kernel x

walk :: Step x -> x -> Int -> Rand -> Action x IO a b -> IO b
walk _ _ 0 _ (Action _ f a) = f a
walk step x n r action = do 
  x' <- step x r
  execute action x' >>= walk step x' (n-1) r

-- Metropolis Hastings --

metropolisHastings :: Kernel (Sample a) a
metropolisHastings t c_p = 
    let mhStep xi g = do
          u <- sampleFrom (uniform [0] [1]) g
          xstar <- sampleFrom (c_p xi) g
          let accept = min 1 (numer / denom)
              numer = density t xstar * density (c_p xstar) xi
              denom = density t xi * density (c_p xi) xstar
          return $ if head u < accept then xstar else xi
    in mhStep

-- Visualizes only the first dimension
vizMH :: PrintF (Sample Double) Double
vizMH = map head

-- Simulated Annealing --

type Temp = Double
type CoolingSchedule = Temp -> Temp
type StateSA a = (a, Temp, CoolingSchedule)

simulatedAnnealing :: Kernel (StateSA (Sample a)) a
simulatedAnnealing t c_p = 
    let saStep (xi,temp,cool) g = do
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

myFilter :: [Sample Double] -> [Sample Double]
myFilter = filter (\x -> x < (repeat 15) && x > (repeat $ -5))

-- Visualizes only the first dimension
vizSA :: PrintF (StateSA (Sample Double)) Double
vizSA = vizMH . myFilter . map tripleFirst

-- Kernel Mixtures --

mixSteps :: MixRatio -> Step x -> Step x -> Step x
mixSteps nu kstep lstep = 
    let mixStep x g = do
          u <- sampleFrom (uniform [0] [1]) g
          if head u < nu then kstep x g else lstep x g
    in mixStep

-- Kernel Cycles --

-- cycleKernel :: Kernel x a -> Target a -> [Sample a -> Proposal a] -> Step x
-- cycleKernel kernel t (cp:cps) =
--   let middles c_p iox g = do x <- iox
--                             return kernel t c_p x g
--       cycleStep x g = 
        