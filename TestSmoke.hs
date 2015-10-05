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
