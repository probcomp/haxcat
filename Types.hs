{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V

import Data.Random.RVar

import Models

newtype RowID = RowID Int deriving (Eq, Ord)
newtype ColID = ColID Int deriving (Eq, Ord)
newtype ViewID = ViewID Int deriving (Eq, Ord, Enum)
newtype ClusterID = ClusterID Int deriving (Eq, Ord, Enum)

-- Can probably get away with making this unboxed
type ColumnData a = V.Vector a

-- TODO What a poor representation of a row!
type Row = M.Map ColID Double

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

view_col_uninc :: ColID -> View -> View
view_col_uninc col_id v@View{view_columns = cs} = v{view_columns = cs'}
    where cs' = M.delete col_id cs

-- ASSUME the column is already correctly partitioned
view_col_reinc :: ColID -> Column -> View -> View
view_col_reinc col_id col v@View{view_columns = cs} = v{view_columns = cs'}
    where cs' = M.insert col_id col cs

-- Treats extra columns in the Row correctly, namely by ignoring them.
-- TODO Tweak to ignore missing columns in the Row also (at fromJust)
view_row_uninc :: RowID -> Row -> View -> View
view_row_uninc r_id row View{..} =
    View view_crp view_counts' view_columns' view_partition' where
        cluster_id = fromJust $ M.lookup r_id view_partition
        view_counts' = remove cluster_id view_counts
        view_partition' = M.delete r_id view_partition
        view_columns' = M.mapWithKey col_uninc view_columns
        col_uninc :: ColID -> Column -> Column
        col_uninc col_id (Column h m) = Column h (M.adjust (remove item) cluster_id m)
            where item = fromJust $ M.lookup col_id row

-- Treats extra columns in the Row correctly, namely by ignoring them.
-- TODO Tweak to ignore missing columns in the Row also (at fromJust)
-- TODO For possible uncollapsed columns, should probably accept a
-- candidate new cluster.
view_row_reinc :: RowID -> Row -> ClusterID -> View -> View
view_row_reinc r_id row cluster_id View{..} =
    case M.lookup r_id view_partition of
      Nothing -> View view_crp view_counts' view_columns' view_partition'
      (Just _) -> error "Reincorporating row id that was not unincorporated"
    where
      view_counts' = insert cluster_id view_counts
      view_partition' = M.insert r_id cluster_id view_partition
      view_columns' = M.mapWithKey col_reinc view_columns
      col_reinc :: ColID -> Column -> Column
      col_reinc col_id (Column h m) = Column h (M.alter add_datum cluster_id m)
          where -- add_datum :: Maybe stats -> Maybe stats
                add_datum stats = Just $ insert item $ fromMaybe empty stats
                item = fromJust $ M.lookup col_id row

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

cc_col_uninc :: ColID -> Crosscat -> Crosscat
cc_col_uninc col_id Crosscat {..} =
    Crosscat cc_crp cc_counts' cc_partition' cc_views' where
        view_id = fromJust $ M.lookup col_id cc_partition
        cc_counts' = remove view_id cc_counts
        cc_partition' = M.delete col_id cc_partition
        cc_views' = M.update flush view_id cc_views
        flush :: View -> Maybe View
        flush = view_nonempty . view_col_uninc col_id

-- - Assume this ColID has already been unincorporated.
-- - ASSUME the column is already correctly partitioned according to
--   the view.
-- - Pass a View object in case this Crosscat has no binding for the
--   ViewID (i.e., if the column is becoming a singleton)
cc_col_reinc :: ColID -> Column -> ViewID -> View -> Crosscat -> Crosscat
cc_col_reinc col_id col view_id view Crosscat{..} =
    case M.lookup col_id cc_partition of
      Nothing  -> Crosscat cc_crp cc_counts' cc_partition' cc_views'
      (Just _) -> error "Reincorporating id that was not unincorporated"
    where
      cc_counts' = insert view_id cc_counts
      cc_partition' = M.insert col_id view_id cc_partition
      cc_views' = M.alter xxx view_id cc_views
      xxx Nothing = Just view
      xxx (Just old_view) = Just $ view_col_reinc col_id col old_view
