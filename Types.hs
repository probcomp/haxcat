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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Types where

import Control.Monad (foldM, liftM)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import qualified Data.Set as S
import Data.Tuple (swap)
import qualified Data.Vector as V

import Data.Random.RVar

import Utils (flipweights, nullify)
import Models

newtype RowID = RowID Int deriving (Eq, Ord)
newtype ColID = ColID Int deriving (Eq, Ord)
newtype ViewID = ViewID Int deriving (Eq, Ord, Enum)
newtype ClusterID = ClusterID Int deriving (Eq, Ord, Enum)

instance Show RowID where
    showsPrec 11 (RowID i) s = "(R " ++ show i ++ ")" ++ s
    showsPrec _  (RowID i) s =  "R " ++ show i ++ s
instance Show ColID where
    showsPrec 11 (ColID i) s = "(Co " ++ show i ++ ")" ++ s
    showsPrec _  (ColID i) s =  "Co " ++ show i ++ s
instance Show ViewID where
    showsPrec 11 (ViewID i) s = "(V " ++ show i ++ ")" ++ s
    showsPrec _  (ViewID i) s =  "V " ++ show i ++ s
instance Show ClusterID where
    showsPrec 11 (ClusterID i) s = "(Cl " ++ show i ++ ")" ++ s
    showsPrec _  (ClusterID i) s =  "Cl " ++ show i ++ s

-- Can probably get away with making this unboxed
type ColumnData a = V.Vector a

data Row = Row (S.Set ColID) (ColID -> Maybe Double)

row_to_map :: Row -> M.Map ColID Double
row_to_map (Row cols cell) = M.fromAscList $ catMaybes $ map cellWithKey $ S.toAscList cols where
    cellWithKey col_id = liftM (col_id,) $ cell col_id

map_to_row :: M.Map ColID Double -> Row
map_to_row m = Row (M.keysSet m) (flip M.lookup m)


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

-- A sequence of items from a CRP b, each associated with an a
-- INVARIANT: The counts have to agree with the results.
data CRPSequence k v = CRPSequence
    { crp_seq_crp :: CRP v
    , crp_seq_counts :: Counts v Int
    , crp_seq_results :: M.Map k v
    }
    deriving Show

crp_seq_uninc :: (Ord k, Ord v) => k -> CRPSequence k v -> CRPSequence k v
crp_seq_uninc key CRPSequence{..} =
    CRPSequence crp_seq_crp crp_seq_counts' crp_seq_results' where
        answer = fromJust $ M.lookup key crp_seq_results
        crp_seq_counts' = remove answer crp_seq_counts
        crp_seq_results' = M.delete key crp_seq_results

crp_seq_reinc :: (Ord k, Ord v) => k -> v -> CRPSequence k v -> CRPSequence k v
crp_seq_reinc key val CRPSequence{..} =
    case M.lookup key crp_seq_results of
      Nothing -> CRPSequence crp_seq_crp crp_seq_counts' crp_seq_results'
      (Just _) -> error "Reincorporating key that was not unincorporated"
    where
      crp_seq_counts' = insert val crp_seq_counts
      crp_seq_results' = M.insert key val crp_seq_results

crp_seq_lookup :: (Ord k) => k -> CRPSequence k v -> Maybe v
crp_seq_lookup key = M.lookup key . crp_seq_results

crp_seq_empty :: (Ord k, Ord v, Enum v) =>
                 CRP v -> [k] -> RVar (CRPSequence k v)
crp_seq_empty crp keys = do
  (partition, counts) <- crp_sample_partition empty crp keys
  return $ CRPSequence crp counts partition

crp_seq_weights :: (Ord v, Enum v) => CRPSequence k v -> [(Double, v)]
crp_seq_weights CRPSequence{..} = crp_weights crp_seq_counts crp_seq_crp

crp_seq_sample :: (Ord v, Enum v) => CRPSequence k v -> RVar v
crp_seq_sample CRPSequence{..} = sample_predictive crp_seq_counts crp_seq_crp

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
    (Show hypers, Show stats, CompoundModel hypers stats Double)
    => Column hypers (M.Map ClusterID stats)

deriving instance Show Column

type Partition = M.Map RowID ClusterID

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

data View = View
    { view_partition :: CRPSequence RowID ClusterID
    , view_columns :: M.Map ColID Column
    }
    deriving Show
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
view_row_uninc r_id (Row _ cell) View{..} =
    View view_partition' view_columns' where
        cluster_id = fromJust $ crp_seq_lookup r_id view_partition
        view_partition' = crp_seq_uninc r_id view_partition
        view_columns' = M.mapWithKey col_uninc view_columns
        col_uninc :: ColID -> Column -> Column
        col_uninc col_id (Column h m) = Column h m'
            where m' = M.alter flush cluster_id m
                  item = fromJust $ cell col_id
                  flush = (>>= (nullify Models.null . remove item))

-- Treats extra columns in the Row correctly, namely by ignoring them.
-- TODO Tweak to ignore missing columns in the Row also (at fromJust)
-- TODO For possible uncollapsed columns, should probably accept a
-- candidate new cluster.
view_row_reinc :: RowID -> Row -> ClusterID -> View -> View
view_row_reinc r_id (Row _ cell) cluster_id View{..} =
    View view_partition' view_columns' where
      view_partition' = crp_seq_reinc r_id cluster_id view_partition
      view_columns' = M.mapWithKey col_reinc view_columns
      col_reinc :: ColID -> Column -> Column
      col_reinc col_id (Column h m) = Column h (M.alter add_datum cluster_id m)
          where -- add_datum :: Maybe stats -> Maybe stats
                add_datum stats = Just $ insert item $ fromMaybe empty stats
                item = fromJust $ cell col_id

view_nonempty :: View -> Maybe View
view_nonempty v@View{view_columns = cs} | M.size cs == 0 = Nothing
                                        | otherwise = Just v

-- The row ids and the entropy are for initializing a random partition
view_empty :: CRP ClusterID -> [RowID] -> RVar View
view_empty crp rows = liftM (flip View M.empty) $ crp_seq_empty crp rows

-- Choice point: are cluster ids unique or shared across columns?
-- - If unique, then partitions are column-specific, and I have to write
--   code to enforce consistency within a view.
-- - If shared, then a cluster ID does not suffice to determine its
--   suff stats, I also need to know the column.
-- Decision: shared.

data Crosscat = Crosscat
    { cc_crp :: CRP ViewID
    , cc_counts :: Counts ViewID Int
    , cc_partition :: M.Map ColID ViewID
    , cc_views :: M.Map ViewID View
    }
    deriving Show

cc_empty :: CRP ViewID -> Crosscat
cc_empty crp = Crosscat crp empty M.empty M.empty

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
      cc_views' = M.alter view_inc view_id cc_views
      view_inc maybe_view = Just $ view_col_reinc col_id col $ fromMaybe view maybe_view

----------------------------------------------------------------------
-- Initialization                                                   --
----------------------------------------------------------------------

per_view_alpha :: Double -- TODO Will want to define a prior and do inference
per_view_alpha = 1

-- TODO Admit heterogeneous columns
-- TODO Will want to define a prior and do inference
-- TODO Setting kappa to 0 implies an infinite-variance prior on the
-- location of the mean; which is great, except that the code is happy
-- to occasionally sample fresh rows from a unique cluster (should it
-- be?), which leads to Infinity data values.
per_column_hypers :: NIGNormal
per_column_hypers = NIGNormal 1 1 1 1

cc_insert_col_from_prior :: ColID -> ColumnData Double -> Crosscat -> RVar Crosscat
-- This is almost the same as col_step (after unincorporation), except
-- it ignores the likelihoods of the existing views and assigns the
-- column according to the prior.  Up to computational efficiency, the
-- effect of inserting with the likelihoods can be recovered by
-- postcomposing this with col_step.
cc_insert_col_from_prior col_id d cc@Crosscat{..} = do
  -- TODO Can we avoid sampling a candidate view if the column gets
  -- added to an existing view?  Will laziness just do that?
  let prior_weights = crp_weights cc_counts cc_crp
  view_id <- flipweights $ map swap prior_weights
  candidate_view <- view_empty new_crp row_ids
  let view = fromMaybe candidate_view $ M.lookup view_id cc_views
      new_col = repartition (crp_seq_results $ view_partition view) d
              $ Column per_column_hypers undefined
  return $ cc_col_reinc col_id new_col view_id view cc
  where
    new_crp = (CRP (ClusterID 0) per_view_alpha)
    row_ids = map RowID [0..V.length d - 1]

cc_alpha :: Double -- TODO Will want to define a prior and do inference
cc_alpha = 1

cc_initialize :: M.Map ColID (ColumnData Double) -> RVar Crosscat
cc_initialize ds = foldM (flip $ uncurry cc_insert_col_from_prior) cc $ M.toList ds where
    cc = cc_empty $ CRP (ViewID 0) cc_alpha
