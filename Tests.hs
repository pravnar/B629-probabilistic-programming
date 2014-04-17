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

chain1 :: Chain
chain1 = (10000, 100)

mh_test_run :: IO (St Double)
mh_test_run = do
  g <- MWC.createSystemRandom
  let act = print_latest_mh_samples 50 (fst chain1)
  walk example_mh_kernel ([0], Left 1) chain1 act g

example_sa_kernel :: SimulatedAnnealing ExampleTarget Normal Double
example_sa_kernel = simulated_annealing ET gaussian_proposal

sa_test_run :: IO (St (Double, Temp, CoolingSchedule))
sa_test_run = do
  g <- MWC.createSystemRandom
  let -- cool_sch t = t / 1.125 :: Temp 
      cool_sch = (*) (1 - 1e-3) :: Temp -> Temp
      init_temp = 1 :: Temp
      initial = [(0, init_temp, cool_sch)]
      act = print_latest_sa_samples 50 (fst chain1)
  walk example_sa_kernel (initial, Left 1) chain1 act g

-- my_filter :: [Double] -> [Double]
-- my_filter = filter (((>) 20) . abs)

main :: IO ()
main = do
  mh_test_run
  -- sa_test_run
  return ()
