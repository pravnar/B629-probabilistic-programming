{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, GADTs, OverlappingInstances #-}

module Distributions ( Distribution (..)
                     , Sampleable (..)
                     , Uniform
                     , uniform
                     , Normal
                     , normal
                     , Normal2
                     , normal2
                     , normal2WithParams
                     , covMatrix
                     , sigma1
                     , sigma2
                     , rho
                     , scalarCovMatrix
                     , TargetMixture
                     , targetMix
                     , ProposalMixture
                     , proposalMix
                     , first
                     , second
                     , replaceWith
                     ) where

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.D
import Control.Monad.Primitive
import Control.Monad

type Probability = Double

class Distribution (d :: * -> *) (a :: *) where
    density :: d a -> a -> Probability

class Distribution d a => Sampleable d a where
    sampleFrom :: (PrimMonad m) => d a -> MWC.Gen (PrimState m) -> m a

-- Uniform -- 

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
    sampleFrom (U a b) = MWC.uniformR (a,b)

instance (Real a, Real b) => Distribution Uniform (a,b) where
    density (U (x1,y1) (x2,y2)) (x,y) = density (U x1 x2) x * density (U y1 y2) y

instance Sampleable Uniform (Double,Double) where
    sampleFrom (U (x1,y1) (x2,y2)) g = do
      x <- sampleFrom (U x1 x2) g
      y <- sampleFrom (U y1 y2) g
      return (x,y)
                                         
-- Normal --

data Normal a = N a a

normal :: (Num a, Ord a, Show a) => a -> a -> Normal a
normal mu sigma 
    | sigma > 0 = N mu sigma
    | otherwise = error $ "Std-dev for Normal distribution must be positive; got " ++ show sigma

instance Distribution Normal Double where
    density (N mu sigma) x = c * exp (n / d)
        where c = 1 / (sigma * (sqrt 2*pi))
              xm = x - mu
              n = negate (xm * xm)
              d =  2 * (sigma * sigma)

instance Sampleable Normal Double where
    sampleFrom (N mu sigma) = MWC.D.normal mu sigma

data Normal2 a = N2 a (a,a)

normal2 :: (Num a, Ord a, Show a) => (a,a) -> ((a,a),(a,a)) -> Normal2 (a,a)
normal2 muse matrix = N2 muse matrix

normal2WithParams :: (Num a, Ord a, Show a) => (a,a) -> (a,a,a) -> Normal2 (a,a)
normal2WithParams muse (sig1,sig2,p)
    | sig1 > 0 && sig2 > 0 = N2 muse ((sig1*sig1, o), (o, sig2*sig2))
    | otherwise = error $ "Sigmas must be non-negative; got " ++ show sig1 ++ " and " ++ show sig2
    where o = p*sig1*sig2

covMatrix :: Normal2 a -> (a,a)
covMatrix (N2 _ m) = m

sigma1 :: Floating a => Normal2 (a,a) -> a
sigma1 = sqrt.fst.fst.covMatrix

sigma2 :: Floating a => Normal2 (a,a) -> a
sigma2 = sqrt.snd.snd.covMatrix

rho :: Floating a => Normal2 (a,a) -> a
rho n = (snd.fst.covMatrix) n / (sigma1 n * sigma2 n)

instance Distribution Normal2 (Double,Double) where
    density n@(N2 (mu1,mu2) _) (x,y) = c1 * exp (c2 * z)
        where c1 = 1 / (2*pi * s1*s2 * sqrt c3)
              c2 = -1 / 2*c3
              (p,s1,s2) = (rho n, sigma1 n, sigma2 n)
              c3 = 1-p*p
              (c4,c5) = ((x-mu1)/s1, (y-mu2)/s2)
              z = c4*c4 - 2*p*c4*c5 + c5*c5

-- According to http://www.statisticalengineering.com/bivariate_normal.htm

instance Sampleable Normal2 (Double,Double) where
    sampleFrom n@(N2 (mu1,mu2) _) g = do
      [z1,z2] <- replicateM 2 $ sampleFrom (N 0 1) g
      let p = rho n
          x = mu1 + (sigma1 n)*z1
          y = mu2 + (sigma2 n)*(z1*p + z2*sqrt(1-p*p))
      return (x,y)

scalarCovMatrix :: Double -> ((Double,Double),(Double,Double))
scalarCovMatrix n = ((n,0),(0,n))

-- Target Mixtures --

type MixRatio = Double

data TargetMixture t u a = TargetMix MixRatio (t a) (u a)

targetMix :: (Distribution t a, Distribution u a) => MixRatio -> t a -> u a -> TargetMixture t u a
targetMix = TargetMix

instance (Distribution t a, Distribution u a) => Distribution (TargetMixture t u) a where
    density (TargetMix nu t u) x = nu*(density t x) + (1-nu)*(density u x)

-- Proposal Mixtures --

data ProposalMixture p q a = ProposalMix MixRatio (p a) (q a)

proposalMix :: (Sampleable p a, Sampleable q a) => MixRatio -> p a -> q a -> ProposalMixture p q a
proposalMix = ProposalMix

instance (Distribution p a, Distribution q a) => Distribution (ProposalMixture p q) a where
    density (ProposalMix nu p q) x = nu*(density p x) + (1-nu)*(density q x)

instance (Sampleable p a, Sampleable q a) => Sampleable (ProposalMixture p q) a where
    sampleFrom (ProposalMix nu p q) g = do
      u <- sampleFrom (uniform 0 1) g
      if u < nu then sampleFrom p g else sampleFrom q g

-- Semantic editor combinators --

-- http://conal.net/blog/posts/semantic-editor-combinators

first  :: (a -> a') -> ((a,b) -> (a',b))
second :: (b -> b') -> ((a,b) -> (a,b'))
 
first  f = \ (a,b) -> (f a, b)
second g = \ (a,b) -> (a, g b)

replaceWith :: a -> (b -> a)
replaceWith x _ = x
