module Main where

import qualified Data.Map as M
import qualified Data.Vector
import Data.Random
import Data.RVar (sampleRVar)

import Types
import Haxcat

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

main :: IO ()
main = do
  cc <- sampleIO $ train (bogodata 1167 23) 1500
  putStrLn $ show cc
  putStrLn "done"

-- Up to the effects of input distribution on performance, satellites is
-- train (bogodata 1167 23) 1500 in 30 minutes

-- Admittedly, this particular data ends up all in one view and all in
-- one cluster, so satellites would be slower (if the thing works)
-- - A column sweep is O(table * num views)
-- - A row sweep is O(table * avg_col num clusters)

bogodata :: Int -> Int -> M.Map ColID (ColumnData Double)
bogodata rows cols = M.fromList $ map column [0..cols-1] where
    column col = (ColID col, Data.Vector.fromList $ map fromIntegral $ map (\x -> x + rows * col) [0..rows-1])
