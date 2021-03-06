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
      e = every 100 a
  walk exampleMH [0] (10^6) g e

exampleSA :: Step (StateSA [Double])
exampleSA = simulatedAnnealing exampleTarget gaussianProposal

saTest :: IO ()
saTest = do
  g <- MWC.createSystemRandom
  let coolSch = (*) (1 - 1e-3) :: Temp -> Temp
      x0 = ([0], 1, coolSch)
      a = batchPrint vizSA 100
      e = every 100 a
  walk exampleSA x0 (10^5) g e

gMix :: Target [Double]
gMix = let g1 = normal [0,0] (diag [1,1])
           g2 = normal [5,5] (diag [2,2])
       in targetMix [g1, g2] [0.3, 0.7] 

prop1 :: [Double] -> Proposal [Double]
prop1 x = updateNth 1 (\y -> normal y [[1]]) x

prop2 :: [Double] -> Proposal [Double]
prop2 x = updateNth 2 (\y -> normal y [[1]]) x

mhMix = let mh1 = metropolisHastings gMix prop1
            mh2 = metropolisHastings gMix prop2
        in mixSteps [mh1, mh2] [0.7, 0.3] 

mixTest :: IO ()
mixTest = do
  g <- MWC.createSystemRandom
  let a = batchPrint vizMH 50
  walk mhMix [0,0] (10^6) g a

mhCycle :: Step [Double]
mhCycle = cycleKernel metropolisHastings gMix [prop1, prop2]

cycleTest :: IO ()
cycleTest = do
  g <- MWC.createSystemRandom
  let a = batchPrint vizMH 50
  walk mhCycle [0,0] (10^6) g a

blockMH :: Step [Double]
blockMH = let target = fromProposal $ normal [0,1,4,7] (diag [2,2,2,2])
              mh4D = metropolisHastings target
              mh1 = mh4D $ updateBlock 1 2 (\y -> normal y (diag [1,1]))
              mh2 = mh4D $ updateBlock 3 3 (\y -> normal y [[1]])
              mh3 = mh4D $ updateNth 4 (\y -> normal y [[1]])
          in mixSteps [mh1, mh2, mh3] [0.5, 0.4, 0.7] 

blockTest :: IO ()
blockTest = do
  g <- MWC.createSystemRandom
  let a = batchPrint vizMH 50
  walk blockMH [0,0,0,0] (10^6) g a

main :: IO ()
main = blockTest

gTest :: IO ()
gTest = do 
  print $ density (prop1 [1,2]) [2,4]
