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
import Test.HUnit hiding (Counts)
import qualified Test.HUnit

import Control.Monad.State
import qualified Data.Map as M
import Data.Random
import Data.RVar (sampleRVar)
import System.Random

import Models
import Types
import Haxcat
import TestUtils
import Predictive
import GewekeTest
import ChiSquare
import DPMMTest

import Plotting.PPPlot
import Graphics.Rendering.Chart.Gtk

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

fixed :: Int -> RVar a -> a
fixed k var = evalState (sampleRVar var) (mkStdGen k)

bogogen :: RVar (Crosscat Double, Row Double)
bogogen = do
  ds <- bogodata2 5 3
  cc <- train ds 5
  row <- cc_sample cc
  return (cc, row)

bogo_cc_row :: (Crosscat Double, Row Double)
bogo_cc_row = fixed 0 bogogen

bogo_cc :: Crosscat Double
bogo_cc = fst bogo_cc_row

bogo_row :: Row Double
bogo_row = snd bogo_cc_row

columns :: Int -> M.Map ColID NIGNormal
columns k = M.fromList $ map col $ [0..k-1] where
    col i = (ColID i, per_column_hypers)

geweke_gen :: RVar [Crosscat Double]
geweke_gen = cc_geweke_chain_instrumented
             [RowID 0, RowID 1] (columns 2) id 3

geweke_gen_2 :: RVar (Crosscat Double)
geweke_gen_2 = cc_geweke_chain [RowID 0, RowID 1, RowID 2] (columns 10) 5

prior_gen_2 :: RVar (Crosscat Double)
prior_gen_2 = cc_predict_full (columns 10) [RowID 0, RowID 1, RowID 2]

agreement :: RVar Double
agreement = do
  prior <- replicateM 500 $ liftM view_count prior_gen_2
  geweke <- replicateM 100 $ liftM view_count geweke_gen_2
  return $ chi_square_p prior geweke

-- TODO Is there a good way to mechanize making plots like this for
-- statistical tests?  Compare `agreement`.
debug_agreement :: IO ()
debug_agreement = do
  prior <- Main.sampleIO $ replicateM 500 $ liftM view_count prior_gen_2
  geweke <- Main.sampleIO $ replicateM 100 $ liftM view_count geweke_gen_2
  toWindow 300 300 $ p_p_plot (Empirical "prior" prior) (Empirical "geweke" geweke)

geweke_gen_3 :: RVar (Crosscat Double)
geweke_gen_3 = cc_geweke_chain [RowID 0] (columns 1) 25

prior_gen_3 :: RVar (Crosscat Double)
prior_gen_3 = cc_predict_full (columns 1) [RowID 0]

-- TODO Encode this in a computational test?  K-S?
debug_agreement_3 :: IO ()
debug_agreement_3 = do
  prior <- Main.sampleIO $ replicateM 2500 (prior_gen_3 >>= cc_sample_col (ColID 0))
  geweke <- Main.sampleIO $ replicateM 500 (geweke_gen_3 >>= cc_sample_col (ColID 0))
  toWindow 300 300 $ p_p_plot (Empirical "prior" prior) (Empirical "geweke" geweke)

tests :: Test
tests = test [ structure_test bogo_cc
             , test $ stable "test/golden/bogo_cc" bogo_cc
             , (M.keys $ row_to_map bogo_row) ~?= [ ColID 0, ColID 1, ColID 2 ]
             , test $ stable "test/golden/bogo_row" $ row_to_map bogo_row
             , True ~=? 0.0 < bogo_predictive
             , True ~=? bogo_predictive < 0.1
             , test $ stable "test/golden/bogo_predictive" bogo_predictive
             , test $ stable "test/golden/geweke_gen" bogo_geweke_cc
             , 10 ~=? length bogo_geweke_row
             , test $ stable "test/golden/geweke_gen_row" bogo_geweke_row
             , True ~=? 0.1 < fixed_agreement
             , test $ stable "test/golden/fixed_agreement" fixed_agreement
             , True ~=? 0 < bogo_kl
             , True ~=? bogo_kl < 0.01
             , test $ stable "test/golden/bogo_kl" bogo_kl
             , True ~=? 0 < bogo_dpmm_kl
             , True ~=? bogo_dpmm_kl < 0.2
             , test $ stable "test/golden/bogo_dpmm_kl" bogo_dpmm_kl
             ]
    where bogo_predictive = cc_pdf_predictive bogo_cc bogo_row
          fixed_agreement = fixed 0 agreement
          bogo_kl = fixed 0 $ estimate_KL_ta two_modes two_modes_ta 300 600
          bogo_dpmm_kl = fixed 0 $ measure_dpmm_kl two_modes 300 20 10 500
          bogo_geweke_cc = fixed 0 geweke_gen
          bogo_geweke_row = row_to_map $ fst
                            $ fixed 0 (geweke_gen_2 >>= (cc_predict (RowID 0)))

main :: IO ()
main = do
  Test.HUnit.Counts { failures = f, errors = e } <- runTestTT $ tests
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
