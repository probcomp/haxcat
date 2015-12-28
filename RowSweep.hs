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

{-# LANGUAGE RecordWildCards #-}

module RowSweep where

import Prelude hiding (mapM)

import Control.Monad (foldM, liftM)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import Data.Traversable (mapM)
import qualified Data.Vector as V

import Data.Random.RVar
import Numeric.Log

import Utils
import Models
import Types

-- Treats missing and extra columns in the Row correctly, namely by
-- ignoring them.
view_weights :: View a -> Row a -> [(ClusterID, Log Double)]
view_weights View{..} row = map likelihood prior_weights where
    prior_weights = crp_seq_weights view_partition
    likelihood :: (Double, ClusterID) -> (ClusterID, Log Double)
    likelihood (w, cluster_id) = (cluster_id, likelihood_one * log_domain w)
        where
          likelihood_one :: Log Double
          likelihood_one = product $ M.elems $ M.intersectionWith col_pdf (row_to_map row) view_columns
          -- col_pdf :: Double -> Column a -> Log Double  (same a)
          col_pdf x (Column hypers m) = pdf_predictive cluster hypers x
              where
                cluster = fromMaybe empty $ M.lookup cluster_id m

-- Treats extra columns in the Row correctly, namely by ignoring them.
-- TODO Will treat missing columns correctly when incorporation and
-- unincorporation do.
view_row_step :: RowID -> Row a -> View a -> RVar (View a)
view_row_step r_id row v@View{..} = do
  let view' = view_row_uninc r_id row v
      weights = view_weights view' row
  cluster_id <- flipweights_ld weights
  return $ view_row_reinc r_id row cluster_id view'

row_step :: RowID -> Row a -> Crosscat a -> RVar (Crosscat a)
row_step r_id row cc@Crosscat{cc_views = views} = do
  views' <- mapM (view_row_step r_id row) views
  return cc{cc_views = views'}

row_sweep1 :: [(RowID, Row a)] -> Crosscat a -> RVar (Crosscat a)
row_sweep1 ds cc = foldM (flip $ uncurry row_step) cc ds

row_sweep2 :: M.Map ColID (ColumnData a) -> Crosscat a -> RVar (Crosscat a)
row_sweep2 ds cc = row_sweep1 (column_major_to_row_major ds) cc

column_major_to_row_major :: M.Map ColID (ColumnData a) -> [(RowID, Row a)]
column_major_to_row_major ds | M.null ds = []
                             | otherwise = map lookup_fun rows where
    rows = map RowID [0..V.length d - 1]
    keys = M.keysSet ds
    (_, d):_ = M.toList ds
    -- lookup_fun :: RowID -> (RowID, Row a) (same a)
    lookup_fun r_id@(RowID idx) =
        (r_id, Row keys (\col_id -> liftM (V.! idx) $ M.lookup col_id ds))

row_major_to_column_major :: [Row a] -> M.Map ColID (ColumnData a)
row_major_to_column_major [] = M.empty
row_major_to_column_major rows@((Row keys _):_) = M.fromList $ zip keys_l data_l
    where
      keys_l = S.toList keys
      data_l = map column keys_l
      column col_id = V.fromList $ map item rows where
          item (Row _ cell) = fromJust $ cell col_id
