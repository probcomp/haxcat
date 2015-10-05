module TestUtils where

import Control.Monad (replicateM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Vector
import Data.Random

import Types
import Predictive

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
