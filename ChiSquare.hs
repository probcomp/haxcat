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

-- The implication from [1] is that two-sample Chi^2 test of sameness
-- is the same as the one-sample Chi^2 test of fit of the disjoint
-- union of the samples to the independent product of the total
-- observed frequencies in the whole data set with the correctly
-- weighted binary distribution on labels.
--
-- In symbols: Given two samples xs :: [a] and ys :: [a], consider
-- the disjoint union
--   union :: [a] -> [b] -> [Either a b]
--   union xs ys = (map Left xs) ++ (map Right ys)
-- If xs and ys are drawn from the same underlying distribution,
-- the following two distributions of type RVar (Either a a) are
-- the same:
--
--   dependently :: RVar a -> RVar a -> Double -> RVar (Either a a)
--   dependently x y p = do
--     label <- bernoulli p
--     if label then do
--       value <- x
--       return $ Left value
--     else do
--       value <- y
--       return $ Right value
--
--   independently :: RVar a -> Double -> RVar (Either a a)
--   independently x p = do
--     label <- categorical $ [(p, Left), (1-p, Right)]
--     value <- x
--     return $ label value
--
-- We treat the disjoint union of our observations as n+m samples from
-- `dependently`, and compare to what we would expect from
-- `independently`, where `p` and the frequencies in `x` are derived
-- from the data (to maximize the likelihood of the observations).
--
-- The analysis suggests that the degrees of freedom are the number of
-- bins - 1, which is the total number of quantities observed minus
-- the number of parameters to fit for the `independently` hypothesis.

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

-- References

-- [1] Jesse Hoey, "The Two-Way Likelihood Ratio (G) Test", June 2012,
-- http://arxiv.org/abs/1206.4881
