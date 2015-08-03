module Test where

import Data.RVar (sampleRVar)
import Data.Random
import Data.Random.RVar
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Normal
import Control.Monad

import Utils
import DPMM

type Sampler = RVar Double
type LogDensity = Double -> Double
type Dist = (Sampler, LogDensity)

mixture_density :: [LogDensity] -> LogDensity
mixture_density ds x = logsumexp (map ($ x) ds) - log (fromIntegral $ length ds)

-- This is a mixture of two Gaussians (-3/1 and 3/1).
two_modes :: Dist
two_modes = (sample, logd) where
    sample = do
      pos_mode <- bernoulli (0.5 :: Double)
      if pos_mode then
          normal 3 1
      else
          normal (-3) 1
    logd x = logsumexp [logPdf (Normal 3 1) x, logPdf (Normal (-3) 1) x] - log 2

estimate_KL :: Dist -> LogDensity -> Int -> RVar Double
estimate_KL from to sample_ct = do
  input <- replicateM sample_ct (fst from)
  return $ (sum $ map term input) / (fromIntegral $ length input)
    where term x = (snd from) x - to x

measure_kl :: Dist -> Int -> Int -> Int -> Int -> RVar Double
measure_kl data_gen train_data_ct iter_ct chain_ct test_ct = do
  input <- replicateM train_data_ct (fst data_gen)
  results <- replicateM chain_ct $ train_dpmm input iter_ct
  let result_density = mixture_density (map predictive_logdensity results)
  estimate_KL data_gen result_density test_ct

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

-- e.g. sampleIO $ measure_kl two_modes 1000 20 10 500
