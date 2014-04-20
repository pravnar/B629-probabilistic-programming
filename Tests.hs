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

mh_test_run :: IO ()
mh_test_run = do
  g <- MWC.createSystemRandom
  let (total,bsize) = (1000000, 50)
      act = mh_batch_viz total bsize
      print_rem = mh_print total
  skip_act <- skip 100 act
  walk example_mh_kernel 0 total g skip_act >>= print_rem.fst.end_skip
  
example_sa_kernel :: SimulatedAnnealing ExampleTarget Normal Double
example_sa_kernel = simulated_annealing ET gaussian_proposal

sa_test_run :: IO ()
sa_test_run = do
  g <- MWC.createSystemRandom
  let -- cool_sch t = t / 1.125 :: Temp 
      cool_sch = (*) (1 - 1e-3) :: Temp -> Temp
      x0 = (0, 1, cool_sch)
      (total,bsize) = (1000000, 50)
      act = sa_batch_viz total bsize
      print_rem = sa_print total
  skip_act <- skip 100 act
  walk example_sa_kernel x0 total g skip_act >>= print_rem.fst.end_skip
       
-- my_filter :: [Double] -> [Double]
-- my_filter = filter (((>) 20) . abs)

main :: IO ()
main = do
  -- mh_test_run
  sa_test_run
  return ()
