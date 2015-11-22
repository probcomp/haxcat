-- -*- coding: utf-8 -*-

--   Copyright (c) 2010-2014, MIT Probabilistic Computing Project
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DPMMTest where

import Data.RVar (sampleRVar)
import Data.Random hiding (sample, pdf, PDF)
import Data.Random.Distribution.Bernoulli
import Control.Monad
import qualified Numeric.Log as Log

import Models
import Utils
import DPMM

data TailAssessable a = forall m. (Model m a) => TailAssessable (RVar m)

mixture_density :: [PDF a] -> PDF a
mixture_density ds x = Log.sum (map ($ x) ds) / log_domain (fromIntegral $ length ds)

two_modes :: CatMixture Double
two_modes = CatMixture [ (0.5, CatComponent $ DistModel $ Normal 3 1)
                       , (0.5, CatComponent $ DistModel $ Normal (-3) 1)]

two_modes_ta :: TailAssessable Double
two_modes_ta = TailAssessable (do
  pos_mode <- bernoulli (0.5 :: Double)
  if pos_mode then
      return $ gaussian 3 1
  else
      return $ gaussian (-3) 1)

gaussian :: Double -> Double -> DistModel (Normal Double)
gaussian mu sig = DistModel $ (Normal mu sig)

estimate_KL :: (Model m a) => m -> PDF a -> Int -> RVar Double
estimate_KL from to sample_ct = do
  input <- replicateM sample_ct (sample from)
  return $ (sum $ map term input) / (fromIntegral $ length input)
    where term x = Log.ln $ (pdf from x) / (to x)

approximately_assess :: Int -> TailAssessable a -> RVar (PDF a)
approximately_assess ct (TailAssessable dist) = do
  assessors <- liftM (map pdf) $ replicateM ct dist
  return $ mixture_density assessors

estimate_KL_ta :: (Model m a) => m -> TailAssessable a -> Int -> Int -> RVar Double
estimate_KL_ta from to latents_ct sample_ct = do
  density <- approximately_assess latents_ct to
  estimate_KL from density sample_ct

-- TODO Is this really the instance I want?  This is the predictive
-- distribution; there may be a good class for the DPMM that captures
-- more of its structure.
instance Model DPMM Double where
    sample _ = return $ error "What?"
    pdf m = Log.Exp . predictive_logdensity m

trained_dpmm :: [Double] -> Int -> TailAssessable Double
trained_dpmm input iters = TailAssessable $ train_dpmm input iters

measure_dpmm_kl :: (Model m Double) => m -> Int -> Int -> Int -> Int -> RVar Double
measure_dpmm_kl data_gen train_data_ct iter_ct chain_ct test_ct = do
  input <- replicateM train_data_ct (sample data_gen)
  estimate_KL_ta data_gen (trained_dpmm input iter_ct) chain_ct test_ct

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar
