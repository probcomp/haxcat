{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Crosscat where

import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Random.RVar
import Numeric.Log

import Models

newtype RowID = RowID Int
newtype ColID = ColID Int
newtype ViewID = ViewID Int deriving Enum
newtype ClusterID = ClusterID Int

-- Can probably get away with making this unboxed
type ColumnData a = V.Vector a

-- Choice point: Are cluster hypers per-cluster or shared across all
-- the clusters in a column?
-- - Decision: The latter seems to make more sense to me, so I'll do that.
data Mixture prior p_stats comp c_stats element =
    ( ComponentModel prior p_stats ClusterID
    , ComponentModel comp c_stats element
    ) => Mixture { prior :: prior
                 , p_stats :: p_stats
                 , c_hypers :: comp
                 , elements :: ColumnData element
                 , assignment :: M.Map RowID ClusterID
                 , components :: M.Map ClusterID c_stats
                 , enumerate :: prior -> [ClusterID] -- TODO Put this in a class?
                 }
-- Invariant: the p_stats have to agree with the assignment (that is,
-- the assignment must be the data set that the p_stats are the
-- statistics of).

instance ComponentModel (Mixture prior p_stats comp c_stats element)
    (NoStat element) element where
    update mix _ = mix
    pdf_marginal = undefined -- Intractable, I think
    pdf_predictive mix x = undefined -- Involves cluster ids and weights, and a Log.sum

-- Note: As written here, Crosscat will not end up being a very nice
-- CRP mixture of CRP mixtures, because either
-- - the inner mixtures would correspond to views, and so would need
--   to be over funny records that span all the columns
-- - or the inner mixtures would correspond to columns, and so would
--   need to share their assignments

data Column = forall hypers stats element.
    (ComponentModel hypers stats element)
    => Column (ColumnData element) hypers (M.Map ClusterID stats)

-- I may want to make the dataset a heterogeneous typed data frame along
-- the lines of https://github.com/acowley/Frames but I am not sure I
-- grok the type-level magic.
-- - Apparently there's also Tables http://hackage.haskell.org/package/tables
-- - And "Carter's Library", referenced from
--   https://www.reddit.com/r/haskell/comments/2dd2um/what_are_some_haskell_alternatives_to_pandasnumpy/,
--   which is allegedly online.
-- - Decision for now: hide the type with an existentials

type Partition = M.Map RowID ClusterID

-- Choice point: are cluster ids unique or shared across columns?
-- - If unique, then partitions are column-specific, and I have to write
--   code to enforce consistency within a view.
-- - If shared, then a cluster ID does not suffice to determine its
--   suff stats, I also need to know the column.
-- Decision: shared.

data Crosscat = Crosscat { xxx :: M.Map ColID ViewID
                         , yyy :: M.Map ColID Column
                         , zzz :: M.Map ViewID Partition
                         }

-- The hyperparameters we may eventualy be interested in include:
-- - alpha (and d?) for the crp on views
-- - alpha (and d?) for each of the per-view crps
-- - per-column, model-type-dependent hyperparameters (presumably
--   shared among that column's clusters)
--   - currently not obvious from baxcat whether the hypers are per
--     column or per cluster; code for doing inference on them is
--     static and accepts a list of all clusters, but the clusters
--     also store copies (which may have to be kept in sync?).

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

-- TODO provided the model accepts the data
recompute_suff_stats :: Partition -> ColumnData d -> (M.Map ClusterID model)
recompute_suff_stats = undefined

repartition :: Partition -> Column -> Column
repartition p (Column d hypers _) =
    Column d hypers $ recompute_suff_stats p d

column_full_p :: Column -> Log Double
column_full_p (Column _ hypers suff_stats) = product marginals where
    marginals = zipWith pdf_marginal (repeat hypers) $ M.elems suff_stats

-- TODO Will eventually want to do hyperparameter inference on this
view_alpha :: Log Double
view_alpha = Exp 0

col_weights :: Column -> Crosscat -> RVar [(ViewID, Log Double)]
col_weights col Crosscat {..} = do
    new_partition <- (undefined :: RVar Partition)
    return $ (new_id, new_p new_partition):existing
        where
          new_id = succ $ fst (M.findMax zzz)
          new_p :: Partition -> Log Double
          new_p new_partition = (column_full_p $ repartition new_partition col)
                                * view_alpha
          existing = [(v_id, (column_full_p $ repartition partition col)
                               * (Exp $ log (fromIntegral $ x v_id))) |
                      (v_id, partition) <- M.toList zzz]
          x :: ViewID -> Int
          x = undefined -- How many columns does this view already
                        -- contain?  (Not counting the one that's
                        -- being moved now)
