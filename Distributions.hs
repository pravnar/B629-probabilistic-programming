{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, GADTs, 
  OverlappingInstances #-}

module Distributions ( AbsCont (..)
                     , Sampleable (..)
                     , Uniform
                     , uniform
                     , Normal
                     , normal
                     , TargetMixture
                     , targetMix
                     , ProposalMixture
                     , proposalMix
                     , first
                     , second
                     , replaceWith
                     ) where

import qualified System.Random.MWC as MWC
import Control.Monad.Primitive
import qualified Data.Packed.Matrix as M
import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified Numeric.Container as C

type Probability = Double

class AbsCont (d :: * -> *) (a :: *) where
    density :: d a -> [a] -> Probability

class AbsCont d a => Sampleable d a where
    sampleFrom :: (PrimMonad m) => d a -> MWC.Gen (PrimState m) -> m [a]

-- Uniform -- 

data Uniform a = U [a] [a]

uniform :: Ord a => [a] -> [a] -> Uniform a
uniform a b
    | b < a = uniform b a
    | a < b = U a b
    | otherwise = error "Wrong parameters for Uniform distribution"

unif1D :: Real a => a -> a -> a -> Probability
unif1D a b x
    | x < a = 0
    | x > b = 0
    | otherwise = 1 / realToFrac (b - a)

instance Real a => AbsCont Uniform a where
    density (U a b) x =
        let tuf f (p,q,r) = f p q r
        in product . map (tuf unif1D) $ zip3 a b x
            
instance Sampleable Uniform Double where
    sampleFrom (U a b) g = mapM (flip MWC.uniformR g) $ zip a b
                                         
-- Normal --

data Normal a = N [a] (M.Matrix a)

normal :: (Ord a, Show a, M.Element a) => [a] -> [[a]] -> Normal a
normal mu cov = N mu (M.fromLists cov)

instance AbsCont Normal Double where
    density (N mu cov) x = c * exp (-d / 2)
        where (covInv, (lndet, sign)) = LA.invlndet cov
              c = 1 / (sqrt $ (exp lndet*sign) * (2*pi) ^^ (length mu))
              xm = C.sub (M.fromLists [x]) (M.fromLists [mu])
              prod = xm C.<> covInv C.<> (M.trans xm)
              d = (M.@@>) prod (0,0)

instance Sampleable Normal Double where
    sampleFrom (N mu cov) g = do
      let l = length mu
          muMat = M.fromLists [mu]
      z <- flip sampleFrom g $ uniform (replicate l 0.0) (replicate l 1.0)
      let zt = M.trans $ M.fromLists [z]
          a = LA.chol cov
      return . head . M.toLists $ C.add muMat $ C.trans $ a C.<> zt

-- Target Mixtures --

type MixRatio = Double

data TargetMixture t u a = TargetMix MixRatio (t a) (u a)

targetMix :: (AbsCont t a, AbsCont u a) => MixRatio -> t a -> u a -> TargetMixture t u a
targetMix = TargetMix

instance (AbsCont t a, AbsCont u a) => AbsCont (TargetMixture t u) a where
    density (TargetMix nu t u) x = nu*(density t x) + (1-nu)*(density u x)

-- Proposal Mixtures --

data ProposalMixture p q a = ProposalMix MixRatio (p a) (q a)

proposalMix :: (Sampleable p a, Sampleable q a) => MixRatio -> p a -> q a -> ProposalMixture p q a
proposalMix = ProposalMix

instance (AbsCont p a, AbsCont q a) => AbsCont (ProposalMixture p q) a where
    density (ProposalMix nu p q) x = nu*(density p x) + (1-nu)*(density q x)

instance (Sampleable p a, Sampleable q a) => Sampleable (ProposalMixture p q) a where
    sampleFrom (ProposalMix nu p q) g = do
      u <- sampleFrom (uniform [0] [1]) g
      if head u < nu then sampleFrom p g else sampleFrom q g

-- Semantic editor combinators --

-- http://conal.net/blog/posts/semantic-editor-combinators

first  :: (a -> a') -> ((a,b) -> (a',b))
second :: (b -> b') -> ((a,b) -> (a,b'))
 
first  f = \ (a,b) -> (f a, b)
second g = \ (a,b) -> (a, g b)

replaceWith :: a -> (b -> a)
replaceWith x _ = x
