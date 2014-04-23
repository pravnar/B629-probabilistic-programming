{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Main where

import Distributions
import Kernels
import Actions
import qualified System.Random.MWC as MWC

data ExampleTarget a = ET

-- Bimodal distribution from section 3.1 of
-- "An Introduction to MCMC for Machine Learning" by C. Andrieu et al.
instance Distribution ExampleTarget Double where
    density ET x = 0.3 * exp (-0.2*x*x) 
                  + 0.7 * exp (-0.2*((x-10)**2))

gaussianProposal :: Double -> Normal Double
gaussianProposal x = normal x 100

exampleMH :: MetropolisHastings ExampleTarget Normal Double
exampleMH = metropolisHastings ET gaussianProposal

mhTest :: IO ()
mhTest = do
  g <- MWC.createSystemRandom
  let a = batchPrint vizMH 100
      s = skip 100 a
  walk exampleMH 0 (10^6) g s
  
exampleSA :: SimulatedAnnealing ExampleTarget Normal Double
exampleSA = simulatedAnnealing ET gaussianProposal

saTest :: IO ()
saTest = do
  g <- MWC.createSystemRandom
  let -- cool_sch t = t / 1.125 :: Temp 
      coolSch = (*) (1 - 1e-3) :: Temp -> Temp
      x0 = (0, 1, coolSch)
      a = batchPrint vizSA 100
      s = skip 100 a
  walk exampleSA x0 (10^6) g s

gauss1 :: Normal2 (Double,Double)
gauss1 = normal2 (0,0) (scalarCovMatrix 1)

gauss2 :: Normal2 (Double,Double)
gauss2 = normal2 (5,5) (scalarCovMatrix 2)

gaussianMix :: TargetMixture Normal2 Normal2 (Double,Double)
gaussianMix = targetMix 0.5 gauss1 gauss2

proposal1 :: (Double,Double) -> Normal2 (Double,Double)
proposal1 (x,_) = normal2 (x,2) (scalarCovMatrix 1)

proposal2 :: (Double,Double) -> Normal2 (Double,Double)
proposal2 (_,y) = normal2 (3,y) (scalarCovMatrix 1)

type MT = MetropolisHastings (TargetMixture Normal2 Normal2) Normal2 (Double,Double)

mh1 :: MT
mh1 = metropolisHastings gaussianMix proposal1

mh2 :: MT
mh2 = metropolisHastings gaussianMix proposal2

mixtureMH :: (Kernel MT (Double,Double), Kernel MT (Double,Double)) => KernelMixture MT MT
mixtureMH = kernelMix 0.7 mh1 mh2

main :: IO ()
main = do
  -- mhTest
  saTest
