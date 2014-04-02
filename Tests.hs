module Main where

import Distributions
import Kernels
import qualified System.Random.MWC as MWC

data ExampleTarget = ET

-- Bimodal distribution from section 3.1 of
-- "An Introduction to MCMC for Machine Learning" by C. Andrieu et al.
instance Distribution ExampleTarget where
    density ET x = 0.3 * exp (-0.2*x*x) 
                  + 0.7 * (exp $ -0.2*((x-10)**2))

gaussian_proposal :: Double -> Normal
gaussian_proposal x = normal x 100

example_mh_kernel :: MetropolisHastings
example_mh_kernel = MH ET gaussian_proposal

test_run :: IO [Double]
test_run = do
  g <- MWC.create
  walk example_mh_kernel [0] 100 100 g

main :: IO ()
main = do 
  samplelist <- test_run
  let l = length samplelist
  putStrLn $ "{\"current_sample\": " ++ show l
               ++ ", \"total_samples\": " ++ show l
               ++ ", \"rvars\": {\"x\": " ++ show samplelist
               ++ "}}"
  -- print samplelist
