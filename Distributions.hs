module Distributions ( Distribution (..)
                     , Sampleable (..)
                     , Uniform
                     , uniform
                     , Normal
                     , normal
                     ) where

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.D
import Control.Monad.Primitive

class Distribution d where
    density :: d -> Double -> Double

class Distribution d => Sampleable d where
    sampleFrom :: (PrimMonad m) => d -> MWC.Gen (PrimState m) -> m Double

data Uniform = U Double Double

uniform :: Double -> Double -> Uniform
uniform a b
    | b < a = uniform b a
    | a < b = U a b
    | otherwise = error "Wrong parameters for Uniform distribution"

instance Distribution Uniform where
    density (U a b) x
        | x < a = 0
        | x > b = 0
        | otherwise = 1 / (b - a)

instance Sampleable Uniform where
    sampleFrom (U a b) g = MWC.uniformR (a,b) g

data Normal = N Double Double

normal :: Double -> Double -> Normal
normal mu sigma 
    | sigma > 0 = N mu sigma
    | otherwise = error $ "Std-dev for Normal distribution must be positive; got " ++ show sigma

instance Distribution Normal where
    density (N mu sigma) x = c * (exp $ n / d)
        where c = 1 / (sigma * (sqrt 2*pi))
              xm = x - mu
              n = - (xm * xm)
              d = 2 * (sigma * sigma)

instance Sampleable Normal where
    sampleFrom (N mu sigma) g = MWC.D.normal mu sigma g