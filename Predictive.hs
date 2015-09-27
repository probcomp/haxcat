{-# LANGUAGE RecordWildCards #-}

module Predictive where

import Prelude hiding (mapM)

import Control.Monad hiding (mapM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Random.RVar
import Data.Traversable

import Models
import Types

view_cluster_sample :: View -> RVar ClusterID
view_cluster_sample View{..} = sample_predictive view_counts view_crp

column_sample :: ClusterID -> Column -> RVar Double
column_sample c_id (Column hypers m) = sample_predictive stats hypers where
  stats = fromJust $ M.lookup c_id m

view_sample :: View -> RVar Row
view_sample v@View{..} = do
  cluster_id <- view_cluster_sample v
  mapM (column_sample cluster_id) view_columns

cc_sample :: Crosscat -> RVar Row
cc_sample Crosscat{..} = liftM (M.foldl' M.union M.empty) per_view where
    per_view = mapM view_sample cc_views
                            
