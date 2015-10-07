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

import Data.Random
import Data.RVar (sampleRVar)

import Haxcat
import TestUtils

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

bogogen :: RVar String
bogogen = do
  ds <- bogodata2 10 5
  cc <- train ds 5
  return $ show cc -- Poor man's force

tests :: Test
tests = test [ sampleIO bogogen >>= putStrLn ] -- Ensure it runs
-- TODO Fix the seed and check equality to a known string

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT $ tests
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
