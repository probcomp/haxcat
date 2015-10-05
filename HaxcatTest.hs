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
