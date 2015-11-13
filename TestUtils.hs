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

module TestUtils where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Data.Random

import Test.HUnit hiding (Counts)

import Models
import Types
import Predictive

-- Only in base 4.8.0.0
sortOn :: Ord a => (b -> a) -> [b] -> [b]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

uniq :: (Eq a) => [a] -> [a]
uniq = map head . group

bogodata :: Int -> Int -> M.Map ColID (ColumnData Double)
bogodata rows cols = M.fromList $ map column [0..cols-1] where
    column col = (ColID col, V.fromList $ map fromIntegral
                 $ map (\x -> x + rows * col) [0..rows-1])

bogodata2 :: Int -> Int -> RVar (M.Map ColID (ColumnData Double))
bogodata2 rows cols = do
  cc <- cc_initialize $ bogodata 0 cols
  new_rows <- replicateM rows (cc_sample cc)
  return $ reshape new_rows
      where
        reshape :: [Row] -> (M.Map ColID (ColumnData Double))
        reshape rows = M.fromList $ map (pick rows) $ map ColID [0..cols-1]
        pick rows col_id = (col_id, V.fromList $ map (lookup col_id) rows)
        lookup col_id (Row _ cell) = fromJust $ cell col_id

class StructureCheckable a where
    structure_test :: a -> Test

instance (Eq c, Num c, Show c) => StructureCheckable (Counts a c) where
    structure_test Counts {..} = test $ total : non_zeros
        where total = counts_total ~?= (sum $ M.elems counts_map)
              non_zeros = map non_zero $ M.elems counts_map
              non_zero e = test $ assertBool "Zero count found" $ not $ e == 0

instance (Ord v, Show v) => StructureCheckable (CRPSequence k v) where
    structure_test CRPSequence {..} =
        test [ ("counts_ok" ~: structure_test crp_seq_counts)
             , ("counts agree" ~: cts)
             ]
        where cts = counts_agree_with_partition crp_seq_counts crp_seq_results

instance StructureCheckable View where
    structure_test View {..} =
        test [ ("partition" ~: structure_test view_partition)
             , ("clusters agree with partition" ~: right_clusters)
             ]
        where right_clusters = map has_right_clusters $ M.elems view_columns
              has_right_clusters (Column _ suffs) =
                  M.keys suffs ~?= (uniq $ sort $ M.elems
                                   $ crp_seq_results view_partition)

counts_agree_with_partition ::
    (Ord a, Show a) => (Counts a Int) -> M.Map k a -> Test
counts_agree_with_partition cs part =
    counts_to_asc_list cs ~?= (sort $ M.elems part)

instance StructureCheckable Crosscat where
    structure_test Crosscat {..} =
        test [ ("partition" ~: structure_test cc_partition)
             , ("views" ~: views_ok)
             , ("all views present" ~: views_there)
             , ("views rectangular" ~: same_rows)
             , ("views have their columns" ~: test right_columns)
             ]
        where
          views_ok = map structure_test $ M.elems cc_views
          views_there = M.keys cc_views ~?= (uniq $ sort $ M.elems
                                            $ crp_seq_results cc_partition)
          same_rows = map sameRows $ map (crp_seq_results . view_partition)
                      $ M.elems cc_views
          right_columns = map rightColumns $ groupBy ((==) `on` snd)
                          $ sortOn snd $ M.toAscList
                          $ crp_seq_results cc_partition
          one_partition = crp_seq_results $ view_partition $ head
                          $ M.elems cc_views
          sameRows m = M.keys m ~?= M.keys one_partition
          rightColumns :: [(ColID, ViewID)] -> Test
          rightColumns cols =
              map fst cols ~=? (M.keys $ view_columns $ fromJust
                               $ M.lookup (snd $ head cols) cc_views)
