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
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V

import Data.Random.RVar
import Numeric.Log

import Utils
import Models

newtype RowID = RowID Int deriving (Eq, Ord)
newtype ColID = ColID Int deriving (Eq, Ord)
newtype ViewID = ViewID Int deriving (Eq, Ord, Enum)
newtype ClusterID = ClusterID Int deriving (Eq, Ord, Enum)

-- Can probably get away with making this unboxed
type ColumnData a = V.Vector a

-- Choice point: Are cluster hypers per-cluster or shared across all
-- the clusters in a column?
-- - Decision: The latter seems to make more sense to me, so I'll do that.
data Mixture prior p_stats comp c_stats element =
    ( CompoundModel prior p_stats ClusterID
    , CompoundModel comp c_stats element
    ) => Mixture { prior :: prior
                 , p_stats :: p_stats
                 , c_hypers :: comp
                 , elements :: ColumnData element
                 , assignment :: M.Map RowID ClusterID
                 , components :: M.Map ClusterID c_stats
                 , enumerate :: prior -> [ClusterID] -- TODO Put this in a class?
                 }
-- INVARIANT: the p_stats have to agree with the assignment (that is,
-- the assignment must be the data set that the p_stats are the
-- statistics of).

instance Model (Mixture prior p_stats comp c_stats element) element where
    pdf mix x = undefined -- Involves cluster ids and weights, and a Log.sum

    -- There is also the aggregate probability of the component
    -- stats, which has no name in this typeclass

-- Note: As written here, Crosscat will not end up being a very nice
-- CRP mixture of CRP mixtures, because either
-- - the inner mixtures would correspond to views, and so would need
--   to be over funny records that span all the columns
-- - or the inner mixtures would correspond to columns, and so would
--   need to share their assignments

-- I may want to make the dataset a heterogeneous typed data frame along
-- the lines of https://github.com/acowley/Frames but I am not sure I
-- grok the type-level magic.
-- - Apparently there's also Tables http://hackage.haskell.org/package/tables
-- - And "Carter's Library", referenced from
--   https://www.reddit.com/r/haskell/comments/2dd2um/what_are_some_haskell_alternatives_to_pandasnumpy/,
--   which is allegedly online.
-- - Decision for now: just work on Doubles.


-- Choice point: Should the component hypers be individual or shared
-- per column?
-- - currently not obvious from baxcat whether the hypers are per
--   column or per cluster; code for doing inference on them is static
--   and accepts a list of all clusters, but the clusters also store
--   copies (which may have to be kept in sync?).
-- - Decision: shared per column (but not across columns with the same
--   type).

data Column = forall hypers stats.
    (CompoundModel hypers stats Double)
    => Column hypers (M.Map ClusterID stats)

type Partition = M.Map RowID ClusterID

data View = View
    { view_crp :: CRP ClusterID
    , view_counts :: Counts ClusterID
    , view_columns :: M.Map ColID Column
    , view_partition :: Partition
    }
-- INVARIANT: The counts have to agree with the partition.
-- INVARIANT: The stats held in each column have to agree with the
--   partition and the (implicit) per-column data.
-- This is a specialization/generalization of Mixture, above:
-- - specialized to CRP (thus doesn't need the enumerate function)
-- - generalized to a map from ColID to Column, which stores the
--   hypers and the component stats; this has the effect of being able
--   to add and remove columns (for which Mixture would need to change
--   the element type)

view_uninc :: ColID -> View -> View
view_uninc col_id v@View{view_columns = cs} = v{view_columns = cs'}
    where cs' = M.delete col_id cs

-- ASSUME the column is already correctly partitioned
view_reinc :: ColID -> Column -> View -> View
view_reinc col_id col v@View{view_columns = cs} = v{view_columns = cs'}
    where cs' = M.insert col_id col cs

view_nonempty :: View -> Maybe View
view_nonempty v@View{view_columns = cs} | M.size cs == 0 = Nothing
                                        | otherwise = Just v

-- The row ids and the entropy are for initializing a random partition
view_empty :: CRP ClusterID -> [RowID] -> RVar View
view_empty crp rows = do
  (partition, counts) <- crp_sample_partition empty crp rows
  return $ View crp counts M.empty partition

-- Choice point: are cluster ids unique or shared across columns?
-- - If unique, then partitions are column-specific, and I have to write
--   code to enforce consistency within a view.
-- - If shared, then a cluster ID does not suffice to determine its
--   suff stats, I also need to know the column.
-- Decision: shared.

data Crosscat = Crosscat
    { cc_crp :: CRP ViewID
    , cc_counts :: Counts ViewID
    , cc_partition :: M.Map ColID ViewID
    , cc_views :: M.Map ViewID View
    }

col_for :: Crosscat -> ColID -> Column
col_for Crosscat {..} c_id = fromJust $ M.lookup c_id (view_columns view)
    where view = fromJust $ M.lookup view_id cc_views
          view_id = fromJust $ M.lookup c_id cc_partition

cc_uninc :: ColID -> Crosscat -> Crosscat
cc_uninc col_id Crosscat {..} =
    Crosscat cc_crp cc_counts' cc_partition' cc_views' where
        view_id = fromJust $ M.lookup col_id cc_partition
        cc_counts' = remove view_id cc_counts
        cc_partition' = M.delete col_id cc_partition
        cc_views' = M.update flush view_id cc_views
        flush :: View -> Maybe View
        flush = view_nonempty . view_uninc col_id

-- - Assume this ColID has already been unincorporated.
-- - ASSUME the column is already correctly partitioned according to
--   the view.
-- - Pass a View object in case this Crosscat has no binding for the
--   ViewID (i.e., if the column is becoming a singleton)
cc_reinc :: ColID -> Column -> ViewID -> View -> Crosscat -> Crosscat
cc_reinc col_id col view_id view Crosscat{..} =
    case M.lookup col_id cc_partition of
      Nothing  -> Crosscat cc_crp cc_counts' cc_partition' cc_views'
      (Just _) -> error "Reincorporating id that was not unincorporated"
    where
      cc_counts' = insert view_id cc_counts
      cc_partition' = M.insert col_id view_id cc_partition
      cc_views' = M.alter xxx view_id cc_views
      xxx Nothing = Just view
      xxx (Just old_view) = Just $ view_reinc col_id col old_view

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

recompute_suff_stats :: (Statistic stat elt) => Partition -> ColumnData elt -> (M.Map ClusterID stat)
recompute_suff_stats p d = M.foldlWithKey' stat_insert M.empty p where
    -- stat_insert :: (M.Map ClusterID stat) -> (RowID, ClusterID) -> M.Map ClusterID stat
    stat_insert m (RowID r_id) c_id = M.alter add_datum c_id m
        where
          -- add_datum :: Maybe stat -> Maybe stat
          add_datum s = Just $ insert (d V.! r_id) $ fromMaybe empty s

repartition :: Partition -> ColumnData Double -> Column -> Column
repartition p d (Column hypers _) =
    Column hypers $ recompute_suff_stats p d

column_full_pdf :: PDF Column
column_full_pdf (Column hypers suff_stats) = product marginals where
    marginals = zipWith pdf_marginal (repeat hypers) $ M.elems suff_stats

col_likelihood :: ColumnData Double -> Column -> View -> Log Double
col_likelihood d col View{..} = column_full_pdf $ repartition view_partition d col

per_view_alpha :: Double -- TODO Will want to define a prior and do inference
per_view_alpha = 1

col_step :: ColID -> ColumnData Double -> Crosscat -> RVar Crosscat
col_step col_id d cc@Crosscat{cc_views = old_view_set} = do
    -- TODO This will propose singleton views even if the column is
    -- already in a singleton view
    let cc'@Crosscat {..} = cc_uninc col_id cc
    candidate_view <- view_empty new_crp row_ids
    let view_for :: ViewID -> View
        view_for v_id = fromMaybe candidate_view $ M.lookup v_id old_view_set
        likelihood :: (Double, ViewID) -> ((ViewID, Column), Log Double)
        likelihood (w, v_id) = ((v_id, new_col), (column_full_pdf new_col) * log_domain w)
              where new_col = repartition (view_partition $ view_for v_id) d col
        prior_weights = crp_weights cc_counts cc_crp
        full_weights = map likelihood prior_weights
    (new_v_id, col') <- flipweights_ld full_weights
    return $ cc_reinc col_id col' new_v_id (view_for new_v_id) cc'

    where col = col_for cc col_id
          new_crp = (CRP (ClusterID 0) per_view_alpha)
          row_ids = map RowID [0..V.length d]
