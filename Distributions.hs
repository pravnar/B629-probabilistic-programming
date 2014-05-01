{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, GADTs, 
  OverlappingInstances #-}

module Distributions ( Rand
                     , Probability
                     , Sample
                     , AbsCont
                     , density
                     , Target (..)
                     , Proposal (..)
                     , sampleFrom
                     , uniform
                     , normal
                     , MixRatio
                     , targetMix
                     , proposalMix
                     , first
                     , second
                     , replaceWith
                     ) where

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.D
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Packed.Matrix as M
import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified Numeric.Container as C

type Rand  = MWC.Gen (PrimState IO)
type Probability = Double
type Sample a = [a]

type Density a = Sample a -> Probability

class AbsCont d a where
    density :: d a -> Density a

data Target a = T (Density a)

instance AbsCont Target a where
    density (T d) = d

type SampleFrom a = Rand -> IO (Sample a)
data Proposal a = P (Density a) (SampleFrom a)

instance AbsCont Proposal a where
    density (P d _) = d

sampleFrom :: Proposal a -> SampleFrom a
sampleFrom (P _ s) = s

-- Uniform -- 

uniform :: (MWC.Variate a, Real a) => Sample a -> Sample a -> Proposal a
uniform a b
    | b < a = uniform b a
    | a < b = makeUniform a b
    | otherwise = error "Wrong parameters for Uniform distribution"

unif1D :: Real a => a -> a -> a -> Probability
unif1D a b x
    | x < a = 0
    | x > b = 0
    | otherwise = 1 / realToFrac (b - a)

makeUniform :: (MWC.Variate a, Real a) => Sample a -> Sample a -> Proposal a
makeUniform a b = 
    let tuf f (p,q,r) = f p q r
        uniD x = product . map (tuf unif1D) $ zip3 a b x
        uniSF g = mapM (flip MWC.uniformR g) $ zip a b
    in P uniD uniSF
                                         
-- Normal --

type CovMatrix = M.Matrix Double
type Mu a = M.Matrix a

mu :: M.Element a => Sample a -> Mu a
mu mean = M.fromLists [mean]

normal :: Sample Double -> [[Double]] -> Proposal Double
normal mean cov =
    let covMat = M.fromLists cov
        (muMat, n) = (mu mean, length mean)
    in P (normalD muMat covMat) (normalSF muMat covMat n)

normalD :: Mu Double -> CovMatrix -> Density Double
normalD m cov x = c * exp (-d / 2)
    where (covInv, (lndet, sign)) = LA.invlndet cov
          c1 = (2*pi) ^^ (length x)
          c = 1 / (sqrt $ sign * (exp lndet) * c1)
          xm = C.sub (M.fromLists [x]) m
          prod = xm C.<> covInv C.<> (M.trans xm)
          d = (M.@@>) prod (0,0)

normalSF :: Mu Double -> CovMatrix -> Int -> SampleFrom Double
normalSF m cov n g = do
      z <- replicateM n (MWC.D.standard g)
      let zt = M.trans $ M.fromLists [z]
          a = LA.chol cov
      return . head . M.toLists $ C.add m $ C.trans $ a C.<> zt

-- Target Mixtures --

type MixRatio = Double

targetMix :: MixRatio -> Target a -> Target a -> Target a
targetMix nu t u = 
    let mixD x = nu*(density t x) + (1-nu)*(density u x)
    in T mixD

-- Proposal Mixtures --

proposalMix :: MixRatio -> Proposal a -> Proposal a -> Proposal a
proposalMix nu p q = 
    let mixD x = nu*(density p x) + (1-nu)*(density q x)
        mixSF g = do
          u <- sampleFrom (uniform [0] [1]) g
          if head u < nu then sampleFrom p g else sampleFrom q g
    in P mixD mixSF

-- Semantic editor combinators --

-- http://conal.net/blog/posts/semantic-editor-combinators

first  :: (a -> a') -> ((a,b) -> (a',b))
second :: (b -> b') -> ((a,b) -> (a,b'))
 
first  f = \ (a,b) -> (f a, b)
second g = \ (a,b) -> (a, g b)

replaceWith :: a -> (b -> a)
replaceWith x _ = x
