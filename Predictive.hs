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

module Predictive where

import Prelude hiding (mapM)

import Control.Monad hiding (mapM)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Random.RVar
import Data.Traversable

import qualified Numeric.Log as Log (sum)

import Models
import Types
import RowSweep (view_weights)

view_cluster_sample :: View -> RVar ClusterID
view_cluster_sample View{..} = crp_seq_sample view_partition

view_cluster_predict :: RowID -> View -> RVar (ClusterID, View)
view_cluster_predict r_id view = do
  (c_id, part') <- crp_seq_predict r_id $ view_partition view
  return (c_id, view{view_partition = part'})

column_sample :: ClusterID -> Column -> RVar Double
column_sample c_id (Column hypers m) = sample_predictive stats hypers where
  stats = fromMaybe empty $ M.lookup c_id m

view_sample :: View -> RVar (M.Map ColID Double)
view_sample v@View{..} = do
  cluster_id <- view_cluster_sample v
  mapM (column_sample cluster_id) view_columns

-- ASSUME The RowID is not incorporated into the suff stats (the
-- partition is checked)
view_predict :: RowID -> View -> RVar (M.Map ColID Double, View)
view_predict r_id view = do
  (c_id, view') <- view_cluster_predict r_id view
  ans <- mapM (column_sample c_id) $ view_columns view'
  let view'' = view_row_only_reinc (map_to_row ans) c_id view'
  return (ans, view'')

cc_sample :: Crosscat -> RVar Row
cc_sample Crosscat{..} = liftM (map_to_row . M.foldl' M.union M.empty) per_view
    where
      per_view = mapM view_sample cc_views

cc_predict :: RowID -> Crosscat -> RVar (Row, Crosscat)
cc_predict r_id Crosscat{..} = do
  -- per_view :: M.Map ViewID (M.Map ColID Double, View)
  per_view <- mapM (view_predict r_id) cc_views
  let cc_views' = M.map snd per_view
      row = map_to_row $ M.foldl' M.union M.empty $ M.map fst per_view
  return (row, Crosscat cc_partition cc_views')

view_pdf_predictive :: View -> PDF Row
view_pdf_predictive view row = Log.sum $ map snd $ view_weights view row

-- Treats missing columns correctly, namely by ignoring them.
cc_pdf_predictive :: Crosscat -> PDF Row
cc_pdf_predictive Crosscat{..} row =
    -- TODO Perhaps this would be faster if I used the cc_partition to
    -- per-filter the data in the row, but this remains correct
    -- because view_predictive ignores extra columns (which maybe it
    -- shouldn't?)
    product $ map (flip view_pdf_predictive row) $ M.elems cc_views

cc_predict_full :: [ColID] -> [RowID] -> RVar Crosscat
cc_predict_full cols rows = do
  partition <- crp_seq_empty (CRP (ViewID 0) cc_alpha) cols
  views <- mapM mk_view $ crp_seq_values partition
  return $ Crosscat partition $ M.fromList $ zip (crp_seq_values partition) views
    where mk_view _ = view_empty (CRP (ClusterID 0) per_view_alpha) rows
