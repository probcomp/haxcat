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

module Query where

import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Data.Random

import qualified Plotting.Stub as Stub
import Predictive (cc_predict_full)
import Types

structurally_dependent :: (Crosscat a) -> ColID -> ColID -> Bool
structurally_dependent cc c1 c2 = v1 == v2 where
    v1 = fromJust $ view_id_for cc c1
    v2 = fromJust $ view_id_for cc c2

dependence_probability_heatmap :: [Crosscat a] -> [ColID] -> Stub.Plot
dependence_probability_heatmap ccs cols =
    Stub.cluster_map "variable 1" "variable 2" cols cols f where
        f c1 c2 = num / denom where
            num = fromIntegral $ length (filter (== True) deps)
            denom = fromIntegral $ length deps
            deps = map (\cc -> structurally_dependent cc c1 c2) ccs

dep_prob_test :: RVar Stub.Plot
dep_prob_test = do
  let cols = [ColID 1, ColID 2, ColID 3]
      rows = []
      act = cc_predict_full cols rows
  ccs <- replicateM 10 act
  return $ dependence_probability_heatmap ccs cols
