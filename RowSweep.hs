{-# LANGUAGE RecordWildCards #-}

module RowSweep where

import Control.Monad (liftM2)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.Random.RVar
import Numeric.Log

import Utils
import Models
import Types

-- Treats missing and extra columns in the Row correctly, namely by
-- ignoring them.
view_weights :: View -> Row -> [(ClusterID, Log Double)]
view_weights View{..} row = map likelihood prior_weights where
    prior_weights = crp_weights view_counts view_crp
    likelihood :: (Double, ClusterID) -> (ClusterID, Log Double)
    likelihood (w, cluster_id) = (cluster_id, likelihood_one * log_domain w)
        where
          likelihood_one :: Log Double
          likelihood_one = product $ M.elems $ mapZipWith (liftM2 col_pdf) row view_columns
          col_pdf :: Double -> Column -> Log Double
          col_pdf x (Column hypers m) = pdf_predictive cluster hypers x
              where
                cluster = fromMaybe empty $ M.lookup cluster_id m

-- Treats extra columns in the Row correctly, namely by ignoring them.
-- TODO Will treat missing columns correctly when incorporation and
-- unincorporation do.
row_step :: RowID -> Row -> View -> RVar View
row_step r_id row v@View{..} = do
  let view' = view_row_uninc r_id row v
      weights = view_weights view' row
  cluster_id <- flipweights_ld weights
  return $ view_row_reinc r_id row cluster_id view'
