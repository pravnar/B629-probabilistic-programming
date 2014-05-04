{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, GADTs, 
  OverlappingInstances #-}

module Distributions ( Rand
                     , Probability
                     , Target (..)
                     , density
                     , Proposal (..)
                     , sampleFrom
                     , fromProposal
                     , uniform
                     , diag
                     , normal
                     , MixRatio
                     , targetMix
                     , proposalMix
                     , first
                     , second
                     , car
                     , cdr
                     , nth
                     , block
                     , swapWith
                     , updateNth
                     , updateBlock
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

type Density a = a -> Probability

data Target a = T (Density a)

class HasDensity d a where
    density :: d a -> Density a

instance HasDensity Target a where
    density (T d) = d

type Sample a = Rand -> IO a
data Proposal a = P (Density a) (Sample a)

instance HasDensity Proposal a where
    density (P d _) = d

sampleFrom :: Proposal a -> Sample a
sampleFrom (P _ s) = s

fromProposal :: Proposal a -> Target a
fromProposal = T . density

-- Uniform -- 

uniform :: (MWC.Variate a, Real a) => [a] -> [a] -> Proposal [a]
uniform a b
    | b < a = uniform b a
    | a <= b = makeUniform a b
    | otherwise = error "Wrong parameters for Uniform distribution"

unif1D :: Real a => a -> a -> a -> Probability
unif1D a b x
    | x < a = 0
    | x > b = 0
    | otherwise = 1 / (max 1 $ realToFrac (b - a))

makeUniform :: (MWC.Variate a, Real a) => [a] -> [a] -> Proposal [a]
makeUniform a b = 
    let tuf f (p,q,r) = f p q r
        uniD x = product . map (tuf unif1D) $ zip3 a b x
        uniSF g = mapM (flip MWC.uniformR g) $ zip a b
    in P uniD uniSF
                                         
-- Normal --

type CovMatrix = M.Matrix Double
type Mu a = M.Matrix a

mu :: M.Element a => [a] -> Mu a
mu mean = M.fromLists [mean]

diag :: [Double] -> [[Double]]
diag d = [(nth i) (swapWith e) (replicate (length d) 0) | (i,e) <- zip [1..] d]

normal :: [Double] -> [[Double]] -> Proposal [Double]
normal mean cov =
    let covMat = M.fromLists cov
        (muMat, n) = (mu mean, length mean)
    in P (normalD muMat covMat) (normalSF muMat covMat n)

normalD :: Mu Double -> CovMatrix -> Density [Double]
normalD m cov x = c * exp (-d / 2)
    where (covInv, (lndet, sign)) = LA.invlndet cov
          c1 = (2*pi) ^^ (length x)
          c = 1 / (sqrt $ sign * (exp lndet) * c1)
          xm = C.sub (M.fromLists [x]) m
          prod = xm C.<> covInv C.<> (M.trans xm)
          d = (M.@@>) prod (0,0)

normalSF :: Mu Double -> CovMatrix -> Int -> Sample [Double]
normalSF m cov n g = do
      z <- replicateM n (MWC.D.standard g)
      let zt = M.trans $ M.fromLists [z]
          a = LA.chol cov
      return . head . M.toLists $ C.add m $ C.trans $ a C.<> zt

-- Target Mixtures --

type MixRatio = Double

targetMix :: (HasDensity t a, HasDensity u a) => 
             MixRatio -> t a -> u a -> Target a
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

car :: (a -> a) -> ([a] -> [a])
cdr :: ([a] -> [a]) -> ([a] -> [a])

car f = \(x:xs) -> f x : xs
cdr f = \(x:xs) -> x : f xs

nth :: Int -> (a -> a) -> ([a] -> [a])
nth 1 = car
nth n = cdr . nth (n-1)

carM :: Monad m => (a -> m a) -> ([a] -> m [a])
carM f (x:xs) = do x' <- f x 
                   return $ x' : xs

cdrM :: Monad m => ([a] -> m [a]) -> ([a] -> m [a])
cdrM f (x:xs) = do xs' <- f xs
                   return $ x : xs'

nthM :: Monad m => Int -> (a -> m a) -> ([a] -> m [a])
nthM 1 = carM
nthM n = cdrM . nthM (n-1)

block :: Int -> Int -> ([a] -> [a]) -> ([a] -> [a])
block begin end f ls
    | begin == end = nth begin (unlift f) ls
    | begin == 1 = f (take end ls) ++ drop end ls
    | otherwise = front ++ f mids ++ back
    where (front, mids, back) = chopAt begin end ls

blockM :: Monad m => Int -> Int -> ([a] -> m [a]) -> ([a] -> m [a])
blockM begin end f ls
    | begin == end = nthM begin (unliftM f) ls
    | begin == 1 = f (take end ls) >>= return . flip (++) (drop end ls)
    | otherwise = do let (front, mids, rest) = chopAt begin end ls
                     fmids <- f mids
                     return $ front ++ fmids ++ rest

swapWith :: a -> (b -> a)
swapWith x _ = x

unlift :: ([a] -> [a]) -> a -> a
unlift f x = head $ f [x]

unliftM :: Monad m => ([a] -> m [a]) -> a -> m a
unliftM f x = f [x] >>= return.head

-- A sample is 1-indexed, i.e., dimensions go from 1 to n
getBlock :: Int -> Int -> [a] -> [a]
getBlock begin end
    | begin == end = \ls -> [ls !! (begin - 1)]
    | otherwise = take (end + 1 - begin) . drop (begin-1)

chopAt :: Int -> Int -> [a] -> ([a], [a], [a])
chopAt begin end ls = (front, mids, back)
    where (front, rest) = splitAt (begin-1) ls
          (mids, back) = splitAt (end + 1 - begin) rest

updateNth :: Int -> ([a] -> Proposal [a]) -> [a] -> Proposal [a]
updateNth n p x = 
    let den y = flip density (getBlock n n y) $ p (getBlock n n x)
        s g = nthM n (unliftM (\xn -> sampleFrom (p xn) g)) x
    in P den s

updateBlock :: Int -> Int -> ([a] -> Proposal [a]) -> [a] -> Proposal [a]
updateBlock n m p x = 
    let den y = flip density (getBlock n m y) $ p (getBlock n m x)
        s g = blockM n m (\b -> sampleFrom (p b) g) x
    in P den s

-- ex = (second.first.second) not (1,((3,True),2))
-- eg = (cdr.cdr.car) not [False,False,True,False]
-- examp = (nth 4) not [False,False,False,True,False]
-- exampl = (nth 4) (swapWith 4) [1,2,3,5,5]
-- diagex = diag [1,2,3,4]
-- cdrex = cdr (\ls -> map ((+)10) ls) [1,2,3,4,5]

