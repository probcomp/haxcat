module Test where

import Data.RVar (sampleRVar)
import Data.Random
import Data.Random.RVar
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Normal
import Control.Monad

import Utils
import DPMM

type LogDensity a = a -> Double
type Assessable a = (RVar a, LogDensity a)
type TailAssessable a = RVar (Assessable a)

mixture_density :: [LogDensity a] -> LogDensity a
mixture_density ds x = logsumexp (map ($ x) ds) - log (fromIntegral $ length ds)

-- This is a mixture of two Gaussians (-3/1 and 3/1).
two_modes :: Assessable Double
two_modes = (sample, logd) where
    sample = do
      pos_mode <- bernoulli (0.5 :: Double)
      if pos_mode then
          normal 3 1
      else
          normal (-3) 1
    logd x = logsumexp [logPdf (Normal 3 1) x, logPdf (Normal (-3) 1) x] - log 2

two_modes_ta :: TailAssessable Double
two_modes_ta = do
  pos_mode <- bernoulli (0.5 :: Double)
  if pos_mode then
      return $ gaussian 3 1
  else
      return $ gaussian (-3) 1

gaussian :: Double -> Double -> Assessable Double
gaussian mu sig = (normal mu sig, logPdf (Normal mu sig))

estimate_KL :: Assessable a -> LogDensity a -> Int -> RVar Double
estimate_KL from to sample_ct = do
  input <- replicateM sample_ct (fst from)
  return $ (sum $ map term input) / (fromIntegral $ length input)
    where term x = (snd from) x - to x

approximately_assess :: Int -> TailAssessable a -> RVar (LogDensity a)
approximately_assess ct dist = do
  assessors <- liftM (map snd) $ replicateM ct dist
  return $ mixture_density assessors

estimate_KL_ta :: Assessable a -> TailAssessable a -> Int -> Int -> RVar Double
estimate_KL_ta from to latents_ct sample_ct = do
  density <- approximately_assess latents_ct to
  estimate_KL from density sample_ct

-- sampleIO $ estimate_KL_ta two_modes two_modes_ta 1000 2000 is ~5e-5

dpmm_dist :: DPMM -> Assessable Double
dpmm_dist dpmm = (return $ error "What?", predictive_logdensity dpmm)

trained_dpmm :: [Double] -> Int -> TailAssessable Double
trained_dpmm input iters = liftM dpmm_dist $ train_dpmm input iters

measure_dpmm_kl :: Assessable Double -> Int -> Int -> Int -> Int -> RVar Double
measure_dpmm_kl data_gen train_data_ct iter_ct chain_ct test_ct = do
  input <- replicateM train_data_ct (fst data_gen)
  estimate_KL_ta data_gen (trained_dpmm input iter_ct) chain_ct test_ct

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

-- e.g. sampleIO $ measure_dpmm_kl two_modes 1000 20 10 500 is ~1-9e-2
