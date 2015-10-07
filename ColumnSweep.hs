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

module ColumnSweep where

import Control.Monad (foldM)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Data.Random.RVar
import Numeric.Log

import Utils
import Models
import Types

----------------------------------------------------------------------
-- Column partition transitions                                     --
----------------------------------------------------------------------

-- BaxCat promises to implement three different column transition
-- kernels, named "Gibbs", "MH", and "Aux Gibbs", but doesn't seem to
-- actually read the parameter that is supposed to choose between
-- them.

-- The actual column transition kernel (for collapsed columns)
-- consists of
-- - Trying every extant view,
-- - If not originally a singleton, trying m fresh singleton views
--   each with an independent crp-random partition (giving each a
--   prior prob of alpha/m) (the per-view crp alpha is chosen
--   uniformly at random)
-- - The likelihood function for each view is taken as full_marginal_logp,
--   which is presumably the probability (density) of the data in the column
--   under the clusters given by that view
-- - Picking the new view according to that distribution without a
--   correction.
--   - 9/25/15: I convinced myself that no correction is required,
--     even for moving a column from its singleton, because the
--     probability of proposing back cancels against the prior penalty
--     for having an extra crp partition around.  (Technically this
--     argument requires proposing the crp alpha from the prior as
--     well).
-- - The difference for uncollapsed columns seems to be that the the
--   per-column cluster objects have states in addition to suffstats
--   and (copies of?) the hyper parameters, which they resample
--   e.g. when the column is reassigned.

column_full_pdf :: PDF Column
column_full_pdf (Column hypers suff_stats) = product marginals where
    marginals = zipWith pdf_marginal (repeat hypers) $ M.elems suff_stats

col_likelihood :: ColumnData Double -> Column -> View -> Log Double
col_likelihood d col View{..} = column_full_pdf $ repartition view_partition d col

col_step :: ColID -> ColumnData Double -> Crosscat -> RVar Crosscat
col_step col_id d cc@Crosscat{cc_views = old_view_set} = do
    -- TODO This will propose singleton views even if the column is
    -- already in a singleton view
    let cc'@Crosscat {..} = cc_col_uninc col_id cc
    candidate_view <- view_empty new_crp row_ids
    let view_for :: ViewID -> View
        view_for v_id = fromMaybe candidate_view $ M.lookup v_id old_view_set
        likelihood :: (Double, ViewID) -> ((ViewID, Column), Log Double)
        likelihood (w, v_id) = ((v_id, new_col), (column_full_pdf new_col) * log_domain w)
              where new_col = repartition (view_partition $ view_for v_id) d col
        prior_weights = crp_weights cc_counts cc_crp
        full_weights = map likelihood prior_weights
    (new_v_id, col') <- flipweights_ld full_weights
    return $ cc_col_reinc col_id col' new_v_id (view_for new_v_id) cc'

    where col = col_for cc col_id
          new_crp = (CRP (ClusterID 0) per_view_alpha)
          row_ids = map RowID [0..V.length d - 1]

col_sweep :: M.Map ColID (ColumnData Double) -> Crosscat -> RVar Crosscat
col_sweep ds cc = foldM (flip $ uncurry col_step) cc $ M.toList ds
