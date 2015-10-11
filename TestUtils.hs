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
import qualified Data.Vector
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
    column col = (ColID col, Data.Vector.fromList $ map fromIntegral $ map (\x -> x + rows * col) [0..rows-1])

bogodata2 :: Int -> Int -> RVar (M.Map ColID (ColumnData Double))
bogodata2 rows cols = do
  cc <- cc_initialize $ bogodata 0 cols
  new_rows <- replicateM rows (cc_sample cc)
  return $ reshape new_rows
      where
        reshape :: [Row] -> (M.Map ColID (ColumnData Double))
        reshape rows = M.fromList $ map pick $ map ColID [0..cols-1]
            where
              pick col_id = (col_id, Data.Vector.fromList $ map (\(Row _ cell) -> fromJust $ cell col_id) rows)

class StructureCheckable a where
    assert_structural :: a -> Assertion

instance (Eq c, Num c, Show c) => StructureCheckable (Counts a c) where
    assert_structural Counts {..} =
        do counts_total @?= (sum $ M.elems counts_map)
           mapM_ non_zero $ M.elems counts_map
               where non_zero e = assertBool "Zero count found" $ not $ e == 0

instance StructureCheckable View where
    assert_structural View {..} =
        do assert_structural view_counts
           assert_counts_agree_with_partition view_counts view_partition
           mapM_ has_right_clusters $ M.elems view_columns
               where has_right_clusters (Column _ suffs) =
                         M.keys suffs @?= (uniq $ sort $ M.elems view_partition)

assert_counts_agree_with_partition ::
    (Ord a, Show a) => (Counts a Int) -> M.Map k a -> Assertion
assert_counts_agree_with_partition cs part =
    counts_to_asc_list cs @?= (sort $ M.elems part)

instance StructureCheckable Crosscat where
    assert_structural Crosscat {..} =
        do assert_structural cc_counts
           assert_counts_agree_with_partition cc_counts cc_partition
           mapM_ assert_structural $ M.elems cc_views
           M.keys cc_views @?= (uniq $ sort $ M.elems cc_partition)
           mapM_ sameRows $ map view_partition $ M.elems cc_views
           mapM_ rightColumns $ groupBy ((==) `on` snd) $ sortOn snd $ M.toAscList cc_partition
               where sameRows m = M.keys m @?= (M.keys $ view_partition $ head $ M.elems cc_views)
                     rightColumns :: [(ColID, ViewID)] -> Assertion
                     rightColumns cols = map fst cols @=? (M.keys $ view_columns $ fromJust $ M.lookup (snd $ head cols) cc_views)
