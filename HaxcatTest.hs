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

import Types (Crosscat)
import Haxcat
import TestUtils

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

bogogen :: RVar Crosscat
bogogen = do
  ds <- bogodata2 5 3
  cc <- train ds 5
  return cc

bogostring :: String
bogostring = "Crosscat {cc_crp = CRP (V 0) 1.0, cc_counts = Counts {counts_map = fromList [(V 5,3)], counts_total = 3}, cc_partition = fromList [(Co 0,V 5),(Co 1,V 5),(Co 2,V 5)], cc_views = fromList [(V 5,View {view_crp = CRP (Cl 0) 1.0, view_counts = Counts {counts_map = fromList [(Cl 3,1),(Cl 6,1),(Cl 7,1),(Cl 8,1),(Cl 9,1)], counts_total = 5}, view_columns = fromList [(Co 0,Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0}) (fromList [(Cl 0,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 3,GaussStats {gauss_n = 1, gauss_mean = 2.563631143649543, gauss_nvar = 0.0}),(Cl 4,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 5,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 6,GaussStats {gauss_n = 1, gauss_mean = 1.3145898907033204, gauss_nvar = 0.0}),(Cl 7,GaussStats {gauss_n = 1, gauss_mean = 0.2486050939519282, gauss_nvar = 0.0}),(Cl 8,GaussStats {gauss_n = 1, gauss_mean = 1.921030057038249, gauss_nvar = 0.0}),(Cl 9,GaussStats {gauss_n = 1, gauss_mean = -1.1249373927282202, gauss_nvar = 0.0})])),(Co 1,Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0}) (fromList [(Cl 0,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 3,GaussStats {gauss_n = 1, gauss_mean = -1.783058357042168, gauss_nvar = -1.1102230246251565e-16}),(Cl 4,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 5,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 6,GaussStats {gauss_n = 1, gauss_mean = 4.073453143716454, gauss_nvar = 0.0}),(Cl 7,GaussStats {gauss_n = 1, gauss_mean = 0.15553733525895386, gauss_nvar = 0.0}),(Cl 8,GaussStats {gauss_n = 1, gauss_mean = 4.2026631704803, gauss_nvar = 0.0}),(Cl 9,GaussStats {gauss_n = 1, gauss_mean = -0.46638032363463977, gauss_nvar = 0.0})])),(Co 2,Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0}) (fromList [(Cl 0,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 3,GaussStats {gauss_n = 1, gauss_mean = 1.4295253756550719, gauss_nvar = 0.0}),(Cl 4,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 5,GaussStats {gauss_n = 0, gauss_mean = NaN, gauss_nvar = NaN}),(Cl 6,GaussStats {gauss_n = 1, gauss_mean = 66.73403208085037, gauss_nvar = 0.0}),(Cl 7,GaussStats {gauss_n = 1, gauss_mean = -6.42991326618917, gauss_nvar = 0.0}),(Cl 8,GaussStats {gauss_n = 1, gauss_mean = 0.7709230346501312, gauss_nvar = 0.0}),(Cl 9,GaussStats {gauss_n = 1, gauss_mean = -0.125360946703319, gauss_nvar = 0.0})]))], view_partition = fromList [(R 0,Cl 3),(R 1,Cl 6),(R 2,Cl 7),(R 3,Cl 8),(R 4,Cl 9)]})]}"

bogo_cc :: Crosscat
bogo_cc = evalState (sampleRVar bogogen) (mkStdGen 0)

tests :: Test
tests = test [ show bogo_cc ~?= bogostring
             , test $ assert_structural bogo_cc
             ]

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT $ tests
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
