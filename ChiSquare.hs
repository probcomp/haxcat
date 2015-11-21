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

{-# LANGUAGE TupleSections #-}

module ChiSquare where

import qualified Data.Map as M

import Data.Random (cdf)
import Data.Random.Distribution.ChiSquare (ChiSquare(..))

import qualified Utils as U

-- Two sample Chi^2 test from
-- http://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/chi2samp.htm

-- Ord for my convenience only.  a is the bin type.
-- Returns the statistic and the measured number of degrees of freedom.
chi_square_stat :: (Eq a, Ord a) => [a] -> [a] -> (Double, Int)
chi_square_stat data1 data2 = (stat, dof) where
    cts1 = histogram data1
    cts2 = histogram data2
    histogram = foldl (flip $ M.alter (U.non 0 (+ 1))) M.empty
    sum1 = fromIntegral $ length data1
    sum2 = fromIntegral $ length data2
    k1 = sqrt (sum2 / sum1)
    k2 = sqrt (sum1 / sum2)
    buckets = pair_with_defaults 0 0 cts1 cts2
    bucket_contrib (ct1, ct2) = (k1 * ct1 - k2 * ct2) ** 2 / (ct1 + ct2)
    stat = sum $ M.elems $ M.map bucket_contrib buckets
    dof = M.size buckets

pair_with_defaults :: (Ord k) => v1 -> v2
                   -> M.Map k v1 -> M.Map k v2 -> M.Map k (v1, v2)
pair_with_defaults def1 def2 =
    M.mergeWithKey (\_ a b -> Just (a, b))
                   (M.map (,def2))
                   (M.map (def1,))

chi_square_p :: (Eq a, Ord a) => [a] -> [a] -> Double
chi_square_p data1 data2 = 1 - cdf (ChiSquare $ fromIntegral dof) stat where
    (stat, dof) = chi_square_stat data1 data2
