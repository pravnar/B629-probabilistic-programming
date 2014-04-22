{-# LANGUAGE MultiParamTypeClasses #-}

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
  s <- skip 100 a
  walk exampleMH 0 (10^6) g s >>= endSkip >>= printRem
  
exampleSA :: SimulatedAnnealing ExampleTarget Normal Double
exampleSA = simulatedAnnealing ET gaussianProposal

saTest :: IO ()
saTest = do
  g <- MWC.createSystemRandom
  let -- cool_sch t = t / 1.125 :: Temp 
      coolSch = (*) (1 - 1e-3) :: Temp -> Temp
      x0 = (0, 1, coolSch)
      a = batchPrint vizSA 100
  s <- skip 100 a
  walk exampleSA x0 (10^6) g s >>= endSkip >>= printRem

main :: IO ()
main = do
  mhTest
  -- saTest
  return ()
