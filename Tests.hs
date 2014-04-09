{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Distributions
import Kernels
import qualified System.Random.MWC as MWC

data ExampleTarget a = ET

-- Bimodal distribution from section 3.1 of
-- "An Introduction to MCMC for Machine Learning" by C. Andrieu et al.
instance Distribution ExampleTarget Double where
    density ET x = 0.3 * exp (-0.2*x*x) 
                  + 0.7 * (exp $ -0.2*((x-10)**2))

gaussian_proposal :: Double -> Normal Double
gaussian_proposal x = normal x 100

example_mh_kernel :: MetropolisHastings ExampleTarget Normal Double
example_mh_kernel = metropolis_hastings ET gaussian_proposal

mh_test_run :: IO [Double]
mh_test_run = do
  g <- MWC.createSystemRandom
  walk example_mh_kernel [0] 100 100 g

example_sa_kernel :: SimulatedAnnealing ExampleTarget Normal Double
example_sa_kernel = simulated_annealing ET gaussian_proposal

sa_test_run :: IO [Double]
sa_test_run = do
  g <- MWC.createSystemRandom
  let cool_sch = (*) (1 - 1e-5) :: Temp -> Temp
      first (a,_,_) = a
      init_temp = 1 :: Temp
  ls <- walk example_sa_kernel [(0, init_temp, cool_sch)] 100 100 g
  return $ map first ls

viz_json :: [Double] -> String
viz_json samplelist = 
    let l = length samplelist
    in "{\"current_sample\": " ++ show l
           ++ ", \"total_samples\": " ++ show l
           ++ ", \"rvars\": {\"x\": " ++ show samplelist
           ++ "}}"

main :: IO ()
main = do 
  mhlist <- mh_test_run
  putStrLn $ viz_json mhlist
  salist <- sa_test_run
  putStrLn $ viz_json salist
