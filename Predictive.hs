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
view_cluster_sample View{..} = sample_predictive view_counts view_crp

column_sample :: ClusterID -> Column -> RVar Double
column_sample c_id (Column hypers m) = sample_predictive stats hypers where
  stats = fromMaybe empty $ M.lookup c_id m

view_sample :: View -> RVar (M.Map ColID Double)
view_sample v@View{..} = do
  cluster_id <- view_cluster_sample v
  mapM (column_sample cluster_id) view_columns

cc_sample :: Crosscat -> RVar Row
cc_sample Crosscat{..} = liftM (map_to_row . M.foldl' M.union M.empty) per_view where
    per_view = mapM view_sample cc_views

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
