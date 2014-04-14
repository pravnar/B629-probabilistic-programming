{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Distributions
import Kernels
import qualified System.Random.MWC as MWC
import Control.Monad

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
  l <- walk example_mh_kernel [0] 100 100 g
  putStrLn $ viz_json l
  return l

example_sa_kernel :: SimulatedAnnealing ExampleTarget Normal Double
example_sa_kernel = simulated_annealing ET gaussian_proposal

sa_test_run :: IO [Double]
sa_test_run = do
  g <- MWC.createSystemRandom
  let cool_sch t = t / 1.125 :: Temp 
      -- cool_sch = (*) (1 - 1e-5) :: Temp -> Temp
      first (a,_,_) = a
      init_temp = 1 :: Temp
  ls <- walk example_sa_kernel [(0, init_temp, cool_sch)] 1000 10 g
  let l = my_filter $ map first ls
  putStrLn $ viz_json l
  return l

my_filter :: [Double] -> [Double]
my_filter = filter (((>) 20) . abs)

viz_json :: [Double] -> String
viz_json samplelist = 
    let l = length samplelist
    in "{\"current_sample\": " ++ show l
           ++ ", \"total_samples\": " ++ show l
           ++ ", \"rvars\": {\"x\": " ++ show samplelist
           ++ "}}"

main :: IO ()
main = do 
  -- replicateM_ 100 mh_test_run
  replicateM_ 100 sa_test_run
  -- salist <- sa_test_run
  -- putStrLn $ viz_json salist
