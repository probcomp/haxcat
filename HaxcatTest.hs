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

module Main where

import System.Exit
import Test.HUnit

import Control.Monad.State
import Data.Random
import Data.RVar (sampleRVar)
import System.Random

import Haxcat
import TestUtils

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

bogogen :: RVar String
bogogen = do
  ds <- bogodata2 5 3
  cc <- train ds 5
  return $ show cc -- Poor man's force

bogostring :: String
bogostring = "Crosscat {cc_crp = CRP (ViewID 0) 1.0, cc_counts = Counts {counts_map = fromList [(ViewID 5,3)], counts_total = 3}, cc_partition = fromList [(ColID 0,ViewID 5),(ColID 1,ViewID 5),(ColID 2,ViewID 5)], cc_views = fromList [(ViewID 5,View {view_crp = CRP (ClusterID 0) 1.0, view_counts = Counts {counts_map = fromList [(ClusterID 3,1),(ClusterID 6,1),(ClusterID 7,1),(ClusterID 8,1),(ClusterID 9,1)], counts_total = 5}, view_columns = fromList [(ColID 0,Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0}) (fromList [(ClusterID 0,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 3,GaussStats {gauss_n = 1, gauss_mean = 2.563631143649543, gauss_nvar = 0.0}),(ClusterID 4,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 5,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 6,GaussStats {gauss_n = 1, gauss_mean = 1.3145898907033204, gauss_nvar = 0.0}),(ClusterID 7,GaussStats {gauss_n = 1, gauss_mean = 0.2486050939519282, gauss_nvar = 0.0}),(ClusterID 8,GaussStats {gauss_n = 1, gauss_mean = 1.921030057038249, gauss_nvar = 0.0}),(ClusterID 9,GaussStats {gauss_n = 1, gauss_mean = -1.1249373927282202, gauss_nvar = 0.0})])),(ColID 1,Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0}) (fromList [(ClusterID 0,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 3,GaussStats {gauss_n = 1, gauss_mean = -1.783058357042168, gauss_nvar = -1.1102230246251565e-16}),(ClusterID 4,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 5,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 6,GaussStats {gauss_n = 1, gauss_mean = 4.073453143716454, gauss_nvar = 0.0}),(ClusterID 7,GaussStats {gauss_n = 1, gauss_mean = 0.15553733525895386, gauss_nvar = 0.0}),(ClusterID 8,GaussStats {gauss_n = 1, gauss_mean = 4.2026631704803, gauss_nvar = 0.0}),(ClusterID 9,GaussStats {gauss_n = 1, gauss_mean = -0.46638032363463977, gauss_nvar = 0.0})])),(ColID 2,Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0}) (fromList [(ClusterID 0,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 3,GaussStats {gauss_n = 1, gauss_mean = 1.4295253756550719, gauss_nvar = 0.0}),(ClusterID 4,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 5,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(ClusterID 6,GaussStats {gauss_n = 1, gauss_mean = 66.73403208085037, gauss_nvar = 0.0}),(ClusterID 7,GaussStats {gauss_n = 1, gauss_mean = -6.42991326618917, gauss_nvar = 0.0}),(ClusterID 8,GaussStats {gauss_n = 1, gauss_mean = 0.7709230346501312, gauss_nvar = 0.0}),(ClusterID 9,GaussStats {gauss_n = 1, gauss_mean = -0.125360946703319, gauss_nvar = 0.0})]))], view_partition = fromList [(RowID 0,ClusterID 3),(RowID 1,ClusterID 6),(RowID 2,ClusterID 7),(RowID 3,ClusterID 8),(RowID 4,ClusterID 9)]})]}"

tests :: Test
tests = test [ evalState (sampleRVar bogogen) (mkStdGen 0) ~?= bogostring ]

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT $ tests
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
