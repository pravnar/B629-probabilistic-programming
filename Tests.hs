module Main where

import Distributions
import Kernels
import Actions
import qualified System.Random.MWC as MWC

-- Bimodal distribution from section 3.1 of
-- "An Introduction to MCMC for Machine Learning" by C. Andrieu et al.
exampleTarget :: Target [Double]
exampleTarget = 
    T $ \[x] -> 0.3 * exp (-0.2*x*x) + 0.7 * exp (-0.2 * (x-10)**2)

gaussianProposal :: [Double] -> Proposal [Double]
gaussianProposal x = normal x [[10000]]

exampleMH :: Step [Double]
exampleMH = metropolisHastings exampleTarget gaussianProposal

mhTest :: IO ()
mhTest = do
  g <- MWC.createSystemRandom
  let a = batchPrint vizMH 50
      s = skip 100 a
  walk exampleMH [0] (10^6) g s

exampleSA :: Step (StateSA [Double])
exampleSA = simulatedAnnealing exampleTarget gaussianProposal

saTest :: IO ()
saTest = do
  g <- MWC.createSystemRandom
  let -- cool_sch t = t / 1.125 :: Temp 
      coolSch = (*) (1 - 1e-3) :: Temp -> Temp
      x0 = ([0], 1, coolSch)
      a = batchPrint vizSA 100
      s = skip 100 a
  walk exampleSA x0 (10^5) g s

g1 :: Target [Double]
g1 = fromProposal $ normal [0,0] (diag [1,1])

g2 :: Target [Double]
g2 = fromProposal $ normal [5,5] (diag [2,2])

gMix :: Target [Double]
gMix = targetMix 0.3 g1 g2

-- p1 :: [Double] -> Proposal [Double]
-- p1 x = updateBlock 1 x $ normal x (diag [1,1])

-- p2 :: [Double] -> Proposal [Double]
-- p2 x = updateBlock 2 x $ normal x (diag [1,1])

-- p1 :: [Double] -> Proposal [Double]
-- p1 x = updateBlock (cdr . swapWith $ tail x) $ normal x (diag [1,1])

proposal :: [Double] -> Proposal [Double]
proposal x = updateNth 1 (\y -> normal y [[1]]) x

-- mh1 = metropolisHastings gMix p1

-- mh2 = metropolisHastings gMix p2

-- mhMix = mixSteps 0.7 mh1 mh2

main :: IO ()
main = mhTest
