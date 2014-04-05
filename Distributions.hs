{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, GADTs #-}

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

type Probability = Double

class Distribution (d :: * -> *) (a :: *) where
    density :: d a -> a -> Probability

class Distribution d a => Sampleable d a where
    sampleFrom :: (PrimMonad m) => d a -> MWC.Gen (PrimState m) -> m a

data Uniform a = U a a

uniform :: Ord a => a -> a -> Uniform a
uniform a b
    | b < a = uniform b a
    | a < b = U a b
    | otherwise = error "Wrong parameters for Uniform distribution"

instance Real a => Distribution Uniform a where
    density (U a b) x
        | x < a = 0
        | x > b = 0
        | otherwise = 1 / realToFrac (b - a)

instance Sampleable Uniform Double where
    sampleFrom (U a b) g = MWC.uniformR (a,b) g

data Normal a = N a a

normal :: (Num a, Ord a, Show a) => a -> a -> Normal a
normal mu sigma 
    | sigma > 0 = N mu sigma
    | otherwise = error $ "Std-dev for Normal distribution must be positive; got " ++ show sigma

instance Distribution Normal Double where
    density (N mu sigma) x = c * (exp $ n / d)
        where c = 1 / (sigma * (sqrt 2*pi))
              xm = x - mu
              n = negate (xm * xm)
              d =  2 * (sigma * sigma)

instance Sampleable Normal Double where
    sampleFrom (N mu sigma) g = MWC.D.normal mu sigma g