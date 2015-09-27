{-# LANGUAGE RecordWildCards #-}

module RowSweep where

import Control.Monad (liftM2)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Numeric.Log

import Utils
import Models
import Types

view_weights :: View -> M.Map ColID Double -> [(ClusterID, Log Double)]
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
