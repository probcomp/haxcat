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

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

fixed :: Int -> RVar a -> a
fixed k var = evalState (sampleRVar var) (mkStdGen k)

bogogen :: RVar (Crosscat, Row)
bogogen = do
  ds <- bogodata2 5 3
  cc <- train ds 5
  row <- cc_sample cc
  return (cc, row)

bogo_cc_expect :: Crosscat
bogo_cc_expect = Crosscat
  { cc_partition = CRPSequence
    { crp_seq_crp = CRP (ViewID 0) 1.0
    , crp_seq_counts = Counts { counts_map = M.fromList [(ViewID 5,3)]
                              , counts_total = 3}
    , crp_seq_results = M.fromList [ (ColID 0, ViewID 5)
                                   , (ColID 1, ViewID 5)
                                   , (ColID 2, ViewID 5)
                                   ]
    }
  , cc_views = M.fromList [(ViewID 5,the_view)]
  } where
    the_view = View
      { view_partition = CRPSequence
        { crp_seq_crp = CRP (ClusterID 0) 1.0
        , crp_seq_counts = Counts
          { counts_map = M.fromList [ (ClusterID 3,1)
                                    , (ClusterID 6,1)
                                    , (ClusterID 7,1)
                                    , (ClusterID 8,1)
                                    , (ClusterID 9,1)]
          , counts_total = 5
          }
        , crp_seq_results = M.fromList [ (RowID 0, ClusterID 3)
                                       , (RowID 1, ClusterID 6)
                                       , (RowID 2, ClusterID 7)
                                       , (RowID 3, ClusterID 8)
                                       , (RowID 4, ClusterID 9)
                                       ]
        }
      , view_columns = M.fromList
        [ (ColID 0, Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0})
           (M.fromList [ (ClusterID 3, GaussStats 1   2.563631143649543   0.0)
                       , (ClusterID 6, GaussStats 1   1.3145898907033204  0.0)
                       , (ClusterID 7, GaussStats 1   0.2486050939519282  0.0)
                       , (ClusterID 8, GaussStats 1   1.921030057038249   0.0)
                       , (ClusterID 9, GaussStats 1 (-1.1249373927282202) 0.0)
                       ]))
        , (ColID 1, Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0})
           (M.fromList [ (ClusterID 3, GaussStats 1 (-1.783058357042168) (-1.1102230246251565e-16))
                       , (ClusterID 6, GaussStats 1   4.073453143716454    0.0)
                       , (ClusterID 7, GaussStats 1   0.15553733525895386  0.0)
                       , (ClusterID 8, GaussStats 1   4.2026631704803      0.0)
                       , (ClusterID 9, GaussStats 1 (-0.46638032363463977) 0.0)
                       ]))
        , (ColID 2, Column (NIGNormal {nign_r = 1.0, nign_nu = 1.0, nign_s = 1.0, nign_mu = 1.0})
           (M.fromList [ (ClusterID 3, GaussStats 1   1.4295253756550719  0.0)
                       , (ClusterID 6, GaussStats 1  66.73403208085037    0.0)
                       , (ClusterID 7, GaussStats 1 (-6.42991326618917)   0.0)
                       , (ClusterID 8, GaussStats 1   0.7709230346501312  0.0)
                       , (ClusterID 9, GaussStats 1 (-0.125360946703319)  0.0)
                       ]))
        ]
      }

bogo_cc_row :: (Crosscat, Row)
bogo_cc_row = fixed 0 bogogen

bogo_cc :: Crosscat
bogo_cc = fst bogo_cc_row

bogo_row :: Row
bogo_row = snd bogo_cc_row

geweke_gen :: RVar [Crosscat]
geweke_gen = cc_geweke_chain_instrumented
             [RowID 0, RowID 1] [ColID 0, ColID 1] id 3

geweke_gen_2 :: RVar Crosscat
geweke_gen_2 = cc_geweke_chain [RowID 0, RowID 1] [ColID 0, ColID 1] 5

prior_gen_2 :: RVar Crosscat
prior_gen_2 = cc_predict_full [ColID 0, ColID 1] [RowID 0, RowID 1]

agreement :: RVar Double
agreement = do
  prior <- replicateM 500 $ liftM view_count prior_gen_2
  geweke <- replicateM 100 $ liftM view_count geweke_gen_2
  return $ chi_square_p prior geweke

-- Basically just checking that it runs (and is deterministic for
-- fixed seed); the actual values here do not represent the result of
-- any analysis.
tests :: Test
tests = test [ bogo_cc ~?= bogo_cc_expect
             , test $ stable "test/golden/bogo_cc" bogo_cc
             , structure_test bogo_cc
             , row_to_map bogo_row ~?= M.fromList [ (ColID 0,-2.6221942999884886)
                                                  , (ColID 1,0.5917825321828307)
                                                  , (ColID 2,-1.3079604690745894)]
             , cc_pdf_predictive bogo_cc bogo_row ~?= 4.49559045282294e-4
             , show (fixed 0 geweke_gen) ~?= "[Crosscat {cc_partition = CRPSequence {crp_seq_crp = CRP (V 0) 1.0, crp_seq_counts = Counts {counts_map = fromList [(V 0,2)], counts_total = 2}, crp_seq_results = fromList [(Co 0,V 0),(Co 1,V 0)]}, cc_views = fromList [(V 0,View {view_partition = CRPSequence {crp_seq_crp = CRP (Cl 0) 1.0, crp_seq_counts = Counts {counts_map = fromList [(Cl 0,2)], counts_total = 2}, crp_seq_results = fromList [(R 0,Cl 0),(R 1,Cl 0)]}, view_columns = fromList []})]},Crosscat {cc_partition = CRPSequence {crp_seq_crp = CRP (V 0) 1.0, crp_seq_counts = Counts {counts_map = fromList [(V 0,2)], counts_total = 2}, crp_seq_results = fromList [(Co 0,V 0),(Co 1,V 0)]}, cc_views = fromList [(V 0,View {view_partition = CRPSequence {crp_seq_crp = CRP (Cl 0) 1.0, crp_seq_counts = Counts {counts_map = fromList [(Cl 0,2)], counts_total = 2}, crp_seq_results = fromList [(R 0,Cl 0),(R 1,Cl 0)]}, view_columns = fromList []})]},Crosscat {cc_partition = CRPSequence {crp_seq_crp = CRP (V 0) 1.0, crp_seq_counts = Counts {counts_map = fromList [(V 0,2)], counts_total = 2}, crp_seq_results = fromList [(Co 0,V 0),(Co 1,V 0)]}, cc_views = fromList [(V 0,View {view_partition = CRPSequence {crp_seq_crp = CRP (Cl 0) 1.0, crp_seq_counts = Counts {counts_map = fromList [(Cl 0,2)], counts_total = 2}, crp_seq_results = fromList [(R 0,Cl 0),(R 1,Cl 0)]}, view_columns = fromList []})]}]"
             , True ~=? 0.1 < fixed 0 agreement
             , 1.7743516890623924e-4 ~=?
               (fixed 0 $ estimate_KL_ta two_modes two_modes_ta 300 600)
             , 0.1199153653396115 ~=?
               (fixed 0 $ measure_dpmm_kl two_modes 300 20 10 500)
             ]

main :: IO ()
main = do
  Test.HUnit.Counts { failures = f, errors = e } <- runTestTT $ tests
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
