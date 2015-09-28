module Main where

import Control.Monad (replicateM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector
import Data.Random
import Data.RVar (sampleRVar)

import Types
import Predictive
import Haxcat

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

bogodata :: Int -> Int -> M.Map ColID (ColumnData Double)
bogodata rows cols = M.fromList $ map column [0..cols-1] where
    column col = (ColID col, Data.Vector.fromList $ map fromIntegral $ map (\x -> x + rows * col) [0..rows-1])

bogodata2 :: Int -> Int -> RVar (M.Map ColID (ColumnData Double))
bogodata2 rows cols = do
  cc <- cc_initialize $ bogodata 0 cols
  new_rows <- replicateM rows (cc_sample cc)
  return $ reshape new_rows
      where
        reshape :: [Row] -> (M.Map ColID (ColumnData Double))
        reshape rows = M.fromList $ map pick $ map ColID [0..cols-1]
            where
              pick col_id = (col_id, Data.Vector.fromList $ map (\(Row _ cell) -> fromJust $ cell col_id) rows)
