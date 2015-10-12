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

import Data.Random
import Data.RVar (sampleRVar)

import Haxcat
import TestUtils

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

main :: IO ()
main = do
  ds <- sampleIO $ bogodata2 1167 23
  cc <- sampleIO $ train ds 15
  putStrLn $ show cc
  putStrLn "done"

-- Up to the effects of input distribution on performance, satellites is
-- train (bogodata 1167 23) 1500 in 30 minutes

-- Admittedly, bogodata ends up all in one view and all in
-- one cluster, so satellites would be slower (if the thing works)
-- - A column sweep is O(table * num views)
-- - A row sweep is O(table * avg_col num clusters)
-- bogodata2 seems to lead to plenty of views and clusters, at least
-- without having debugged inference.
