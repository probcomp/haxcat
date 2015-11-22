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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module DPMM where

import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Random.RVar
import Numeric.Log
import qualified Numeric.Log as Log (sum)

import Models
import Utils

-- Dirichlet Process Mixture (of NIGs)

newtype ClusterID = ClusterID Int deriving (Eq, Ord, Show, Enum)
newtype DatumID = DatumID Int deriving (Eq, Ord, Show)

data DPMM = DPMM { components :: M.Map ClusterID GaussStats
                 , assignment :: M.Map DatumID ClusterID
                 , dataset :: M.Map DatumID Double
                 } deriving Show

-- Facade

train_dpmm :: [Double] -> Int -> RVar DPMM
train_dpmm input iters =
    execStateT (replicateM iters gibbsSweepT) $ bogoinit input

-- Initialization

-- Put everything into one component
bogoinit :: [Double] -> DPMM
bogoinit dataset = DPMM components assignment dataset'
    where components = (M.singleton (ClusterID 0) component)
          component =  foldl (flip insert) empty dataset
          assignment = M.fromList [ (DatumID idx, ClusterID 0)
                                    | idx <- [0..length dataset - 1]]
          dataset' = M.fromList (zip (map DatumID [0..]) dataset)

-- Predictive log density for a new datum

predictive_logdensity :: DPMM -> Double -> Double
predictive_logdensity dpmm datum = ln $ Log.sum weights where
  correction = log_domain $ fromIntegral $ M.size $ dataset dpmm
  weights = map (\x -> x / correction) $ map snd $ getweights datum dpmm

-- Gibbs sweeps (on cluster assignments)

-- Gibbs sweep, reassigning every datum
gibbsSweep :: DPMM -> RVar DPMM
gibbsSweep dpmm = execStateT gibbsSweepT dpmm

gibbsSweepT :: StateT DPMM RVar ()
gibbsSweepT = do
  datums <- liftM M.keys $ gets dataset
  mapM_ stepT datums

stepT :: DatumID -> StateT DPMM RVar ()
stepT idx = do
    dpmm <- get
    dpmm' <- lift $ step idx dpmm
    put dpmm'

-- reassign one datum (by DatumID)
step :: DatumID -> DPMM -> RVar DPMM
step idx dpmm = do
    let (datum, dpmm') = uninc idx dpmm
    let weights = getweights datum dpmm'
    clusterid <- flipweights_ld weights
    return $ reinc datum idx clusterid dpmm'

dpmm_hypers :: NIGNormal
dpmm_hypers = NIGNormal 1 1 1 0

dpmm_crp_alpha :: Log Double
dpmm_crp_alpha = 1

-- Weight of the datum in every component (inc. a new one)
getweights :: Double -> DPMM -> [(ClusterID, Log Double)]
getweights datum DPMM{..} = new:existing
    where new = (succ $ fst (M.findMax components),
                 pdf_predictive empty dpmm_hypers datum * dpmm_crp_alpha)
          existing = [(componentIdx, pdf_predictive stats dpmm_hypers datum *
                                     log_domain (fromIntegral (gauss_n stats)))
                      | (componentIdx, stats) <- M.toList components]

-- unincorporate a datum from its component (and return its value)
uninc :: DatumID -> DPMM -> (Double, DPMM)
uninc idx DPMM{..} = (datum, DPMM components' (M.delete idx assignment) (M.delete idx dataset))
    where components' = M.alter flush componentIdx components
          datum = fromJust $ M.lookup idx dataset
          componentIdx = fromJust $ M.lookup idx assignment
          flush = (>>= (nullify Models.null . remove datum))

-- reincorporate a datum into the given component
reinc :: Double -> DatumID -> ClusterID -> DPMM -> DPMM
reinc datum datumIdx componentIdx DPMM{..} = DPMM components' assignment' dataset'
    where assignment' = M.insert datumIdx componentIdx assignment
          dataset' = M.insert datumIdx datum dataset
          components' = M.alter add_datum componentIdx components
          add_datum = Just . insert datum . fromMaybe empty
